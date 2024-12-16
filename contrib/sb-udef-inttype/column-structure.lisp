;;; Define a column-structure.
;;
;; The structure name is used to create a UDEF-INTTYPE,
;; the slot definitions are used to create accessor
;; functions similar to DEFSTRUCT.
;; Data is stored in vectors per slot, so the number of Lisp objects
;; is much smaller than with DEFSTRUCT.
;; Referencing such "instances" is done via the (typed!) UDEF.
;;
;; You can either create one set of (large) vectors,
;; which will mean some load when resizing
;; (and concurrent modification while resizing will lose changes!);
;; or a 2-level structure, where the CONTENT-VECTOR
;; stores ("heap"-)vectors to slot-vectors ;)
;; In the latter case allocating more data means just modifying
;; the "heap"-vectors addressed by CONTENT-VECTOR,
;; so that should be multithreading-safe.

;; The biggest advantage of that scheme is that with specialized slots
;; the GC load and memory usage is reduced, as vectors with such types
;; are very quickly handled.
;;
;; Also, one advantage using the user-defined integer types
;; over just allocating big (UNSIGNED-BYTE 8) buffers and
;; defining accessor functions over integer indices
;; is that this solution is runtime-type-safe --
;; as at any point the compiler knows which data type hides
;; behind some element index.

;; The use-case this is being implemented for is a database -
;; data gets imported into the heap, so that SAVE-LISP-AND-DIE
;; gives an instant-on service that (when saved uncompressed)
;; pages data in on demand -- even if only 2GB RAM are available
;; for a 20GB heap dump.

;; The savings in GC time for s-l-a-d are quite nice:
;;   # make comparison dumpfile=/tmp/my-binary
;;   DEFSTRUCT:
;;     Elapsed:   0:19.25   User: 18.08   System: 1.11
;;     Size:    554694544   Created:    16000000
;;   DEF-COLUMN-STRUCT:
;;     Elapsed:   0:06.66   User: 5.31   System: 1.33
;;     Size:    448297152   Used items: 16000001 of 16896304
;;   DEF-COLUMN-STRUCT with large initial-size:
;;     Elapsed:   0:05.05   User: 3.83   System: 1.22
;;     Size:    432465384   Used items: 16000001 of 16240000
;;   DEF-COLUMN-STRUCT with large initial-size and batched:
;;     Elapsed:   0:06.60   User: 5.83   System: 0.73
;;     Size:    452164960   Used items: 16000001 of 17000017

;; Keywords: headerless-structures, column-oriented-data, gc-friendly, runtime-type-safety
(in-package :sb-udef-inttype)


(declaim (optimize sb-c:instrument-consing))


;; Sizing heuristics for parallel allocation in threads
(defun default-batch-size (guess)
  (expt 2 (ceiling (log guess 2) 4/3)))

;; for ATOMIC-INCF to work, this must be a structure.
;; The slot names (rather their initargs) might collide with ones our users choose, sadly...
(defstruct (udef-c-s-metadata
             (:conc-name cs-meta-)
             )
  (udef            nil :type symbol      :read-only t)
  (next              0 :type sb-vm:word)
  (allocated         0 :type sb-vm:word)
  (as-alist        nil :type symbol      :read-only t)
  (batch-size      nil :type (or null
                                 ;; should actually start at 1000 or so,
                                 ;; but race tests run with small increments
                                 (integer 3 1000000000))
                   ;; Allow modification during test
                       :read-only nil)
  (lock            (sb-thread:make-mutex :name "c-s-upper-lock")
                       :type sb-thread:mutex
                       :read-only t)
  (slots           nil :type vector      :read-only t)
  (data-vec        #() :type simple-vector :read-only t))

(defclass cs-slot ()
  ((index         :initform 0   :initarg :index        :type fixnum        :reader cs-s-index        )
   (slot-name     :initform nil :initarg :slot-name    :type symbol        :reader cs-s-slot-name    )
   (accessor-sym  :initform nil :initarg :accessor-sym :type symbol        :reader cs-s-accessor-sym )
   ;; User-supplied data
   (orig-type     :initform nil :initarg :orig-type    :type t             :reader cs-s-orig-type    )
   (init-fn       :initform nil :initarg :init-fn      :type t             :reader cs-s-init-fn      )
   (init-form     :initform nil :initarg :init-form    :type t             :reader cs-s-init-form    )))

(defclass cs-s-array  (cs-slot)
  ;; vectors can be serialized
  ((array-dim     :initform   0 :initarg :array-dim     :type sb-int:index  :reader cs-s-array-dim    )
   (array-el-type :initform nil :initarg :array-el-type :type t             :reader cs-s-array-el-type)))

(defclass cs-s-udef (cs-slot)
  ((udef-sym      :initform nil :initarg :udef-sym      :type symbol        :reader cs-s-udef-sym     )
   (u-slot-type   :initform nil :initarg :u-slot-type   :type list          :reader cs-s-u-slot-type  )
   ))

(defclass cs-s-udef-vector (cs-s-array cs-s-udef)
  ;; NIY
  ())


(defmacro load-time-slot-vector (sym)
  `(load-time-value (cs-meta-data-vec
                      (get-cs-metadata-from-symbol ,sym))))

(defmacro load-time-col-struct (sym)
  `(load-time-value (get-cs-metadata-from-symbol ,sym)))

(defmacro load-time-udef-struct (sym)
  `(load-time-value
     (get-udef-metadata-from-symbol ,sym)))


(defmethod cs-s-array-dim ((slot cs-slot))
  nil)

(defgeneric cs-s-storage-type (item)
  (:documentation
   "Returns the _storage_ type for ITEM.")
  (:method ((slot cs-slot))
    (cs-s-orig-type slot))
  (:method ((slot cs-s-array))
    (cs-s-array-el-type slot))
  (:method ((u udef-metadata))
    `(unsigned-byte
       ,(udef-metadata-max-bits u)))
  (:method ((s symbol))
    (cs-s-storage-type
      (get-udef-metadata-from-symbol s)))
  (:method ((slot cs-s-udef))
    (cs-s-storage-type (cs-s-udef-sym slot))))

(defgeneric cs-s-input-type (item)
  (:documentation
   "Returns the _input_ type for ITEM.")
  (:method ((slot cs-slot))
    (cs-s-orig-type slot))
  (:method ((slot cs-s-udef))
    `(or ,(cs-s-orig-type slot)
         null)))

(defgeneric single-initial-element (slot)
  (:method ((slot cs-slot))
    (funcall (cs-s-init-fn slot)))
  (:method ((slot cs-s-udef))
    (funcall (udef-metadata-store-udef
               (get-udef-metadata-from-symbol (cs-s-udef-sym slot)))
             (funcall (cs-s-init-fn slot))))
  (:method ((slot cs-s-array))
    (aref (call-next-method) 0)))

(defun get-udef-metadata-from-symbol (sym)
  (let ((v (and
             (symbolp sym)
             (get sym 'udef-metadata))))
    (when v
      (the udef-metadata v))))

(defun get-cs-metadata-from-symbol (sym &optional c-s-req)
  (cond
    ((symbolp sym)
     (let ((v-c-s (get sym 'column-struct-data))
           (udef  (get-udef-metadata-from-symbol sym)))
       (if (and (null v-c-s)
                c-s-req)
           (error "~s is not a column-structure type." sym)
           (values (and v-c-s
                        (the udef-c-s-metadata v-c-s))
                   udef))))
    ((udef-c-s-metadata-p sym)
     (values sym
             (get-udef-metadata-from-symbol (cs-meta-udef sym))))))

;(declaim (inline get-new-id-range))
(defun get-new-id-range (obj &optional (count 1))
  (sb-ext:atomic-incf (cs-meta-next obj)
                      count))

(declaim (ftype (function (t) (sb-int:index)) column-struct-size))
(defun column-struct-size (obj)
  "Returns the allocated length (not the number of elements used)"
  (if (symbolp obj)
      (column-struct-size
        (get-cs-metadata-from-symbol obj t))
      (cs-meta-allocated obj)))


(defun column-struct-last-index (obj)
  "Returns the last used index;
  doesn't care about fragmentation with batched allocation."
  (if (symbolp obj)
      (column-struct-last-index
        (get-cs-metadata-from-symbol obj t))
    ;; In case this is called in an AFTER-RESIZE-HOOK,
    ;; ensure we're not going outside allocations
      (min (cs-meta-allocated obj)
           (cs-meta-next      obj))))


(defun column-struct-get-struct (obj &key (output :alist))
  "Returns the data of OBJ as an alist."
  (declare (ignore output))
  (multiple-value-bind (type idx) (udef-inttype-type-of obj)
    ;; Optimizations possible
    (declare (ignore idx))
    (assert type)
    (let ((c-s (get-cs-metadata-from-symbol type)))
      (funcall (cs-meta-as-alist c-s)
               obj))))

(defun reduce-element-counts (vec new)
  (loop for slot across vec
        for i upfrom 0
        do (setf (aref vec i)
                 (adjust-array slot (list new)
                               :element-type (array-element-type slot)))))

(defgeneric after-resize-hook (sym c-s)
  (:method (a b)
    (declare (ignore a b))
    t))

(defun column-struct-resize (c-s new-size &key force-smaller)
  "Resizes to (at least) NEW-SIZE.
  Calls concurrency-unsafe ADJUST-ARRAY for a single-level column-struct,
  or adds/removes batches for a two-level column-struct."
  (declare (optimize (sb-c:instrument-consing 0)))
  (sb-thread:with-mutex ((cs-meta-lock c-s))
    (sb-vm:without-arena
      (cond
        ((< new-size (cs-meta-allocated c-s))
         ;; reduce size
         (when force-smaller
           ;; but only when forced to!
           (let ((new-count (if (cs-meta-batch-size c-s)
                                ;; Set number of batches (rounded up).
                                (ceiling new-size (cs-meta-batch-size c-s))
                                ;; or just length of slot vectors.
                                new-size)))
             ;; TODO: CAS? shouldn't happen concurrently with allocations?
             (setf (cs-meta-next c-s)
                   (min (cs-meta-next c-s)
                   new-size))
             (setf (cs-meta-allocated c-s)
                   new-size)
             (sb-vm:%write-barrier)
             (reduce-element-counts (cs-meta-data-vec c-s)
                                    new-count))))
        ((= new-size (cs-meta-allocated c-s))
         ;; nothing to do after acquiring mutex
         t)
        (t
         ;; enlarge.
         (let* ((2-level? (cs-meta-batch-size c-s))
                (new-vec-size (if 2-level?
                                  (ceiling new-size (cs-meta-batch-size c-s))
                                  new-size)))
           (loop for slot-def across (cs-meta-slots c-s)
                 for slot-vec across (cs-meta-data-vec c-s)
                 for slot-nr = (cs-s-index slot-def)
                 for orig-type = (cs-s-orig-type slot-def)
                 ;for slot-type = (or (cs-s-trslt-type slot-def)
                 ;                    (cs-s-array-el-type slot-def)
                 ;                    orig-type
                 ;                    (error "No type"))
                 for slot-type = (cs-s-storage-type slot-def)
                 for elem-per-slot = (or (cs-s-array-dim slot-def)
                                         1)
                 for init-val = (funcall (cs-s-init-fn slot-def))
                 for new-elements = (- new-vec-size (array-dimension slot-vec 0))
                 do (setf (aref (cs-meta-data-vec c-s)
                                slot-nr)
                          (if 2-level?
                              ;; create a number of batches
                              (let* ((new (loop repeat new-elements
                                                collect (make-array (* elem-per-slot
                                                                       (cs-meta-batch-size c-s))
                                                                    :element-type slot-type
                                                                    :adjustable nil
                                                                    :fill-pointer nil
                                                                    :displaced-to nil
                                                                    :initial-element (single-initial-element slot-def)
                                                                    )))
                                     (data (concatenate 'list slot-vec new)))
                                (make-array new-vec-size
                                            :element-type (array-element-type slot-vec)
                                            :initial-contents data))
                              (let* ((new (make-list (* elem-per-slot new-elements)
                                                     :initial-element (single-initial-element slot-def)))
                                     (data (concatenate 'list slot-vec new)))
                                (make-array (* elem-per-slot
                                               new-vec-size)
                                            :element-type slot-type
                                            :initial-contents data)))))
           (sb-vm:%write-barrier)
           (setf (cs-meta-allocated c-s)
                 (if 2-level?
                     (* new-vec-size (cs-meta-batch-size c-s))
                     new-size))
           (sb-vm:%write-barrier)
           (after-resize-hook (cs-meta-udef c-s)
                              c-s)
           )))))
  (column-struct-size c-s))

(defun column-struct-reset (obj)
  "Soft-resets OBJ, ie. sets the last used index to 0."
  (cond
    ((symbolp obj)
     (column-struct-reset
       (get-cs-metadata-from-symbol obj t)))
    (t
     ;; Do that first, so that C-S-VALUES can be used in a AFTER-RESIZE-HOOK
     (column-struct-resize obj 0 :force-smaller t)
     )))

(defun column-struct-clear (obj &key new-initial-size)
  "Hard-resets OBJ, ie. drops storage vectors."
  (if (symbolp obj)
      (column-struct-reset
        (get-cs-metadata-from-symbol obj t))
      (progn
        (column-struct-reset obj)
        (column-struct-resize obj 0 :force-smaller t)
        (when new-initial-size
          (column-struct-resize obj new-initial-size))
        new-initial-size)))


(defun return-batch-macro (col-struct macro-name
                                      constructor arg-list args)
  (sb-int:with-unique-names (next left new-batch data-var)
    `(defmacro ,macro-name ((fn-name &key batch-size new-batch-cb) &body body)
       ,(format nil "FN-NAME is a local constructor taking the same arguments as the constructor of
                ~s, with a locally reserved range of IDs so that threads operate
                on different cache lines." col-struct)
       (check-type fn-name symbol)
       (sb-ext:with-current-source-form (body)
         `(let ((,',next 0)
                (,',left 0)
                (,',data-var (load-time-col-struct ',',col-struct)))
            (labels ((,',new-batch ()
                       ;; TODO: try to exactly fill up, so that other threads
                       ;; or the next calls have clean batches just for themselves?
                       (setf ,',left (or ,batch-size (cs-meta-batch-size ,',data-var)))
                       (setf ,',next (get-new-id-range ,',data-var ,',left))
                       (when ',new-batch-cb
                         (funcall ,new-batch-cb ,',next ,',left)))
                     (,fn-name (&key ,@', arg-list)
                       (let* ((id ,',next))
                         (when (<= ,',left 0)
                           (,',new-batch)
                           (setf id ,',next))
                         (incf ,',next)
                         (decf ,',left)
                         (,',constructor ,',data-var id ,@', args))))
              (locally
                ,@ body)))))))

(defun parse-slot (input)
  (declare (sb-ext:muffle-conditions style-warning))
  (destructuring-bind (name &optional init &key (type t))
      (sb-int:ensure-list input)
    (values name init type)))

(defun is-simple-1dim-array (type)
  (ignore-errors
    ;; restricted to a subset of possible array-types
    (destructuring-bind (kind element-type (dimension)) type
      (when (and (member kind '(array simple-array))
                 (numberp dimension))
        (multiple-value-bind (c-s element-udef)
            (get-cs-metadata-from-symbol element-type)
          (declare (ignore c-s))
          (list element-type
                dimension
                element-udef))))))

;; Getters --------------------------------------------------
;;
;; TODO: check-type
;; Without the VALUES we get
;;   note: Type assertion too complex to check efficiently:
;;     (VALUES SB-INT:UDEF-INTTYPE &REST T).
;;   It allows an unknown number of values, consider using
;;     (VALUES SB-INT:UDEF-INTTYPE &OPTIONAL).

(defmethod cs-getter-fn-form (struct-name (slot cs-slot))
  (with-slots (accessor-sym slot-name) slot
  `(defun ,accessor-sym (id)
     (declare (optimize (speed 3) (safety 1) (debug 1))
              (type ,struct-name id))
     (with-c-s-slots (,struct-name id) (,slot-name)
       (values ,slot-name)))))

(defmethod cs-getter-fn-form (struct-name (slot cs-s-udef))
  (with-slots (accessor-sym slot-name) slot
    `(defun ,accessor-sym (id &key raw?)
       (declare (optimize (speed 3) (safety 1) (debug 1))
                (type ,struct-name id))
       (declare (ignore raw?)) ; TODO
       (with-c-s-slots (,struct-name id) (,slot-name)
         ;; TODO raw
         (values ,slot-name)))))

(defmethod cs-getter-fn-form (struct-name (slot cs-s-array))
  (with-slots (accessor-sym slot-name) slot
  `(defun ,accessor-sym (id &key start end index (mode :displaced))
     (declare (optimize (speed 3) (safety 1) (debug 1))
              (type ,struct-name id))
     (declare (ignore mode)) ; TODO
     (with-c-s-slots (,struct-name id) (,slot-name)
       (cond
         (index
          (values (aref ,slot-name index)))
         (start
          ;; TODO: fold into MAKE-ARRAY
          (values (subseq ,slot-name start end)))
         (t
          (values ,slot-name)))))))

;; Setters --------------------------------------------------
;;
;; TODO: Type for NEW?

(defmethod cs-setter-fn-form (struct-name (slot cs-slot))
  (with-slots (accessor-sym slot-name) slot
    `(defun (setf ,accessor-sym) (new id)
       (declare (optimize (speed 3) (safety 1) (debug 1))
                (type ,struct-name id))
       (with-c-s-slots (,struct-name id) (,slot-name)
         (setf ,slot-name
               new)))))

(defmethod cs-setter-fn-form (struct-name (slot cs-s-udef))
  (with-slots (accessor-sym slot-name) slot
    `(defun (setf ,accessor-sym) (new id &key raw?)
       (declare (optimize (speed 3) (safety 1) (debug 1))
                (type ,struct-name id))
       (declare (ignore raw?)) ; TODO
       (with-c-s-slots (,struct-name id) (,slot-name)
         (setf ,slot-name
               new)))))

(defmethod cs-setter-fn-form (struct-name (slot cs-s-array))
  (with-slots (accessor-sym slot-name) slot
    `(defun (setf ,accessor-sym) (new id &key index)
       (declare (optimize (speed 3) (safety 1) (debug 1))
                (type ,struct-name id))
       (with-c-s-slots (,struct-name id) (,slot-name)
         (cond
           (index
            (setf (aref ,slot-name index)
                  new))
           (t
            ;; TODO: start1, end1, etc.?
            (replace ,slot-name new)
            new))))))

(defun get-slot-def (struct-name index input)
  (multiple-value-bind (name init user-type) (parse-slot input)
    (multiple-value-bind (c-s meta) (get-cs-metadata-from-symbol user-type)
      (declare (ignore c-s))
      (destructuring-bind (&optional arr-element-type arr-len arr-udef)
          (is-simple-1dim-array user-type)
        (multiple-value-bind (type+args)
            (cond
              (arr-element-type
               (when arr-udef
                 (error "Array of UDEFs not implemented yet"))
               ;; TODO: doesn't work on arrays-of-arrays and similar
               (unless (and (consp init)
                            (eq (first init) 'make-array))
                 (setf init
                       `(make-array (list ,arr-len)
                                    :element-type ',arr-element-type
                                    :initial-element ,init)))
               `('cs-s-array
                     :array-dim ,arr-len
                     :array-el-type ',arr-element-type))
              (meta
                #+(or)
               (setf init
                     `(,(udef-metadata-store-udef meta)
                        ,init))
               `('cs-s-udef
                     :udef-sym ',user-type
                     :u-slot-type ',(cs-s-storage-type meta)))
              (t
               `('cs-slot)))
        `(make-instance ,@ type+args
          :index        ,index
          :slot-name    ',name
          :accessor-sym ',(sb-int:symbolicate struct-name "-" name)
          :orig-type    ',user-type
          :init-form    ',init
          ;; default
          :init-fn      (sb-ext:with-current-source-form (',input)
                          (lambda () ,init))))))))


(defun handle-c-s-slots (struct-name slot-input)
  (loop with new-val = (gensym "NEW-VAL")
        for s-i in slot-input
        for i upfrom 0
        for slot-form = (get-slot-def struct-name i s-i)
        ;; precedent: (sb-xc:defmacro DEFCLASS) in defclass.lisp
        for slot = (eval slot-form)
        for slot-name = (cs-s-slot-name slot)
        ;;
        collect slot into slots
        collect slot-form into slot-forms
        collect (sb-int:gensymify* slot-name :-INIT) into tmp-names
        ;;
        ;; This initarg is user-visible, use supplied value and translate later
        collect (list slot-name
                      (cs-s-init-form slot)) into maker-args
        collect `(check-type ,slot-name
                             ,(cs-s-input-type slot)) into maker-checks
        finally (return (values slots slot-forms
                                maker-args maker-checks
                                tmp-names))))


(declaim (notinline ref-udef-vec
                 (setf ref-udef-vec)))

(defun ref-udef-vec (vec index udef-tag to-nil-value)
  (let ((v (aref vec index)))
    (if (= v to-nil-value)
        nil
        (sb-impl:make-twice-tagged-udef udef-tag v))))

(defun (setf ref-udef-vec) (new vec index udef-tag to-nil-value)
  (setf (aref vec index)
        (if (null new)
            to-nil-value
            (sb-impl:check-tagged-udef-value udef-tag new)))
  new)


(defgeneric cs-with-cs-form (cs slot vec idx)
  (:documentation
   "Returns a list of bindings, a list of declarations,
   a list of FLETs, and a SYMBOL-MACROLET body form, as a list."))

(defmethod cs-with-cs-form (cs (slot cs-slot) vec idx)
  `(()
    ((type (simple-array ,(cs-s-orig-type slot) (*)) ,vec))
    ()
    (aref ,vec ,idx)))

(defmethod cs-with-cs-form (cs (slot cs-s-array) vec idx)
  (sb-int:with-unique-names (scaled-idx fn content)
    `(((,scaled-idx (* ,idx ,(cs-s-array-dim slot)))
       (,content (make-array (list ,(cs-s-array-dim slot))
                  :element-type ',(cs-s-array-el-type slot)
                  :displaced-to ,vec
                  :displaced-index-offset ,scaled-idx)))
      ((type (simple-array ,(second
                              (cs-s-orig-type slot))
                           (*))
             ,vec))
      ((,fn ()
            ,content)
       ((setf ,fn) (new)
                   (replace ,content new)))
      (,fn)
      )))

(defmethod cs-with-cs-form (cs (slot cs-s-udef) vec idx)
  `(()
    ((type (simple-array ,(cs-s-u-slot-type slot) (*)) ,vec))
    ()
    ;; TODO: always translates to/from NIL
    (ref-udef-vec ,vec ,idx
                  ,(sb-impl:udef-metadata-udef-id
                     (get-udef-metadata-from-symbol
                       (cs-s-udef-sym slot)))
                  ,(1- (ash 1
                            (udef-metadata-max-bits
                              (get-udef-metadata-from-symbol
                                (cs-s-udef-sym slot))))))))


;; Hook into CL:WITH-SLOTS?
;; sb-mop:slot-value-using-class?
(defmacro with-c-s-slots ((udef-name index) names &body body)
  "Installs symbol-macros that access NAMES from UDEF-NAME
  at INDEX (which can resp. should be a UDEF).
  NAMES can be a list of slot names or a list of (SYM SLOT)."
  (multiple-value-bind (c-s def) (get-cs-metadata-from-symbol udef-name t)
    (sb-int:with-unique-names (val iidx slots-vec batch-idx inner-idx c-s-var)
      `(let* ((,iidx ,index)
              (,val (cond
                      ((numberp ,iidx)
                       ,iidx)
                      ((,(udef-metadata-type-p def) ,iidx)
                       (,(udef-metadata-store-udef def) ,iidx))
                      (t (error "wrong input ~s, expect ~s udef"
                                ,iidx ',(cs-meta-udef c-s)))))
              (,c-s-var (load-time-col-struct ',udef-name))
              (,slots-vec (load-time-slot-vector ',udef-name)))
         (unless (typep ,val ',(cs-s-storage-type def))
           (error "Bad index value ~s, wanted a ~s for ~s"
                  ,val
                  ',(cs-s-storage-type def)
                  ',(cs-meta-udef c-s)))
         (multiple-value-bind (,batch-idx ,inner-idx)
             ,(if (cs-meta-batch-size c-s)
                `(floor ,val (cs-meta-batch-size ,c-s-var))
                `(values nil ,val))
           ,(loop for entry in names
                  for e-list = (sb-int:ensure-list entry)
                  for var-name = (first e-list)
                  for slot-name = (or (second e-list) var-name)
                  for slot = (or (find slot-name
                                       (cs-meta-slots c-s)
                                       :test #'eq
                                       :key #'cs-s-slot-name)
                                 (error "Invalid slot name ~s" slot-name))
                  for batch-var = (sb-int:gensymify* slot-name :-batch)
                  for vec-var = (sb-int:gensymify* slot-name :-data-vec)
                  for (lets% decls% flets% body%) = (cs-with-cs-form c-s slot vec-var inner-idx)
                  collect `(,batch-var (aref ,slots-vec ,(cs-s-index slot))) into lets
                  collect `(,vec-var (if ,batch-idx
                                         (aref ,batch-var ,batch-idx)
                                         ,batch-var)) into lets
                  appending lets% into lets
                  appending decls% into decls
                  appending flets% into flets
                  collect `(,var-name ,body%) into s-macros
                  finally (return
                            `(let* ,lets
                               (declare ,@ decls)
                               (flet , flets
                                 (symbol-macrolet ,s-macros
                                   ,@body))))))))))


(defmacro def-column-struct (name-and-options &rest slots)
  "Like DEFSTRUCT, but creates an array per slot,
  and the \"identity\" of an instance is actually the index
  in the arrays, returned as a user-defined integer type.

  Redefinition looses all old data (like with DEFSTRUCT,
  though for different reasons.).

  Available options:
  - :INITIAL-SIZE to avoid reallocations
  - :CONSTRUCTOR
  - :UDEF-INTTYPE-ID
  - :MAX-BITS
  - :BATCHED gives lower batch size for a 2-level allocation;
  with T a default is chosen from INITIAL-SIZE
  - :WITH-BATCH-MACRO provides a macro that does batch allocations,
  for use in threads

  Advantages:
  - GC only sees a few big arrays, so much less work to do
  - smaller instance \"pointers\" (depending on number of items 32bit or even smaller)
  Restrictions:
  - Garbage collection is not available (only reset of all data)
  TODO - Is not a class, so PRINT-OBJECT etc. can't be specialized yet
  "
  (destructuring-bind (struct-name &rest options) (sb-int:ensure-list name-and-options)
    (flet ((option (name default)
             (or (second (find name options :key #'first)) default)))
      ;; These structures may reference other instances of the same type;
      ;; so honour pre-declared UDEFs.
      (multiple-value-bind (old-id old-from-u old-to-u old-typep old-bits)
          (get-existing-udef-f-t-p-b struct-name)
        (sb-int:with-unique-names (old-size idx where)
          (let* ((initial-size (option :initial-size 16384))
                 (batched% (option :batch-size nil))
                 (batch-size (if (eq batched% t)
                                 (default-batch-size initial-size)
                                 batched%))
                 ;;
                 (from-udef (or old-from-u
                                (option :from-udef
                                        (sb-int:gensymify* struct-name :-from-udef))))
                 (to-udef (or old-to-u
                              (option :to-udef
                                      (sb-int:gensymify* struct-name :-to-udef))))
                 (udef-typep (or old-typep
                                 (option :udef-typep
                                         (sb-int:gensymify* struct-name :-type-p))))
                 (max-bits (or old-bits
                               (option :max-bits 32)))
                 ;;
                 (base-constructor (option :base-constructor (sb-int:gensymify* struct-name :-constructor)))
                 ;;; (slots-vec-sym  (option :slots-vec-sym (sb-int:gensymify* struct-name :-slots-vec)))
                 ;;
                 (with-batch-macro (option :with-batch-allocation-name nil))
                 (constructor-name (option :constructor
                                           (intern (format nil "~a~a" :make- struct-name)
                                                   (symbol-package struct-name))))
                 ;;
                 (as-alist-sym (sb-int:gensymify* struct-name :-as-alist)))
            (multiple-value-bind (slots slot-defs
                                        maker-arg-list maker-checks
                                        tmp-names)
                (handle-c-s-slots struct-name slots)
              (when (zerop (length slot-defs))
                (error "Need at least one slot in ~s" struct-name))
              (let ((slot-names (mapcar #'cs-s-slot-name slots))
                    (tmp (gensym))
                    (cs-def `(make-udef-c-s-metadata
                                     :udef          ',struct-name
                                     :batch-size    ,batch-size
                                     :slots         (vector ,@ slot-defs)
                                     :data-vec      (vector ,@(loop for s in slots
                                                                    collect #()))
                                     ;;;:slots-vec-sym ',slots-vec-sym
                                     :as-alist      ',as-alist-sym)))
                ;; Make that available now, and return the form for load-time as well
                (eval cs-def)
                ;;
                `(progn
                   (eval-when (:compile-toplevel :load-toplevel :execute)
                     ;; won't redefine if udef already exists
                     (def-udef-inttype ,struct-name
                       :id ,old-id
                       :max-bits ,max-bits
                       ;; NIL gets stored in U-B columns as -1 (mod bits) values
                       :nil-as-minus-1 t ; TODO configurable?
                       :typep ,udef-typep
                       :to-udef ,to-udef
                       :from-udef ,from-udef)
                     ;;;
                     ;;; The elements of the slot-vectors have different types.
                     ;;;(declaim (type (simple-array t (*)) ,slots-vec-sym)
                     ;;;         (sb-ext:global ,slots-vec-sym))
                     ;;;(sb-vm:without-arena
                     ;;;  (setf ,slots-vec-sym
                     ;;;        (make-array ,(length slot-defs)
                     ;;;                    :element-type '(simple-array t (*))
                     ;;;                    :initial-contents (loop repeat ,(length slot-defs)
                     ;;;                                            collect #()))))
                     ;;;(declaim (sb-ext:always-bound ,slots-vec-sym))
                     ;;;;;
                     ;; TODO: loses value upon reload, keep old contents?
                     (sb-vm:without-arena
                       (setf (get ',struct-name 'column-struct-data)
                             ,cs-def))
                     ;;
                     #+(or)(declaim (inline ,base-constructor))
                     (defun ,base-constructor (,where ,idx ,@ tmp-names)
                       (declare (optimize (debug 1)
                                          (speed 3)
                                          (compilation-speed 0)
                                          (safety 1))
                                (type (integer 0 ,(expt 2 max-bits)) ,idx)
                                (type udef-c-s-metadata ,where))
                       ;; TODO: provide restart for reallocation
                       (locally
                         (declare (notinline column-struct-size))
                         (loop for ,old-size = (column-struct-size ,where)
                               while (>= ,idx ,old-size)
                               for i from 0 below 5
                               do (column-struct-resize
                                    ,where
                                    ;; 1 level: exponential growth;
                                    ;; 2-level: only one (batch-size) element added
                                    (if ,(and batch-size t)
                                        (+ ,idx
                                           (cs-meta-batch-size ,where))
                                        (max (round (* (sqrt 2)
                                                       ,idx))
                                             (+ ,idx
                                                50))))))
                       (when (>= ,idx (column-struct-size ,where))
                         (error "Cannot resize ~s" ',struct-name))
                       (with-c-s-slots (,struct-name ,idx) ,slot-names
                         ,@(loop for name in slot-names
                                 for initarg in tmp-names
                                 collect `(setf ,name ,initarg)))
                       ;; Return specifically tagged UDEF-INTTYPE
                       (,to-udef ,idx))
                     ;; User-visible constructor function
                     (defun ,constructor-name (&key ,@ maker-arg-list)
                       ,@ maker-checks
                       ;; Value before incrementing
                       (let* ((,tmp (load-time-col-struct ',struct-name))
                              (,idx (get-new-id-range ,tmp)))
                         (,base-constructor ,tmp ,idx ,@ slot-names)))
                     ;;
                     ;;;(declaim ,@ decl)
                     ;;;,@ code
                     ,@ (loop for slot in slots
                              collect (cs-getter-fn-form struct-name slot)
                              collect (cs-setter-fn-form struct-name slot))
                     ;;
                     (defun ,as-alist-sym (,tmp)
                       (with-c-s-slots (,struct-name ,tmp) ,slot-names
                         (list
                           ,@(loop for name in slot-names
                                   collect (intern (symbol-name name)
                                                   :keyword)
                                   collect name))))
                     ;;
                     ,(when with-batch-macro
                        ;; Sanity check
                        (when (not batch-size)
                          (error "WITH-BATCHED-ALLOCATION is only available in batch mode (yet)."))
                        (assert (> batch-size 3)) ;; TODO increase
                        (return-batch-macro struct-name with-batch-macro
                                            base-constructor
                                            maker-arg-list slot-names))
                     ;;
                     ;; BROKEN: There is no class named X.
                     #+(or)
                     (defmethod sb-c::describe-object :after ((obj ,struct-name) stream)
                       (with-c-s-slots (,struct-name ,idx) slot-names
                         ,@(loop for name in slot-names
                                 collect `(format stream "~&  ~A = ~A~%" ',name ,name)))))
                   ;;
                   (let ((,tmp (load-time-col-struct ',struct-name)))
                     (column-struct-reset ,tmp)
                     (column-struct-resize ,tmp ,initial-size))
                   ',struct-name)))))))))


#+(or)
(defmacro make-wrapped-udef-accessor (new old col-struct-name)
  "Wraps the (UNSIGNED x) value returned by (OLD obj) in (NEW obj)
  so that it becomes COLUMN-STRUCT-TYPE.
  If there's a (SETF OLD), a corresponding wrapper is created as well."
  (declare (ignore col-struct-name)) ;; TODO
  (let* ((fn-type (sb-introspect:function-type old))
         (ret% (third fn-type))
         (ret-type (if (and (consp ret%)
                            (eq 'values (first ret%)))
                       (second ret%)
                       ret%))
         (old-setf (fboundp `(setf ,old))))
    (multiple-value-bind (c-s udef) (get-cs-metadata-from-symbol obj)
      (assert udef)
      (assert (eq (first ret-type) 'unsigned-byte))
      ;; Must have space for at least as many bits
      (assert (>= (second ret-type) (cs-meta-max-bits udef)))
      `(progn
         (declaim (inline ,new)
                  ;(ftype (function ,(second fn-type)) (values ,col-struct-name)) ,new)
                  )
         ;; TODO: &rest or other args possible? Not on a getter, right?
         (defun ,new (obj)
           (,(sb-impl:udef-metadata-to-udef udef)
             (,old obj)))
         ,(when old-setf
            (let ((old-args (sb-introspect:function-lambda-list old-setf)))
              (multiple-value-bind (flags req opt rest keys)
                  (sb-int:parse-lambda-list old-args)
                (declare (ignore flags))
                (assert (null keys))
                ;; declaim ftype?
                `(defun (setf ,new) ,old-args
                   (setf (,old ,@ (cdr req)
                               ,@ opt
                               ,@ rest)
                         (,(sb-impl:udef-metadata-from-udef udef) ,(first req)))
                   ,(first req)))))
         ',new))))


;; TODO: only map really used entries? WITH-BATCH-MACRO might skip some. need a PK?
(defun map-c-s-range (fn col-struct)
  "Runs MAPCAR over all defined values for COL-STRUCT,
  calling FN with the UDEF."
  (multiple-value-bind (c-s udef) (get-cs-metadata-from-symbol col-struct t)
    (loop for max = (column-struct-last-index c-s)
          for i upfrom 0
          ;; Not a BELOW, because the size may change during iteration
          while (< i max)
          for u = (funcall (udef-metadata-to-udef udef) i)
          collect (funcall fn u))))

;; TODO: only map really used entries? WITH-BATCH-MACRO might skip some. need a PK?
(defun c-s-values (col-struct)
  "Returns a list of all defined COL-STRUCT values."
  (multiple-value-bind (c-s udef) (get-cs-metadata-from-symbol col-struct t)
    (loop for i upfrom 0
          ;; Not a BELOW, because the size may change during iteration
          while (< i (column-struct-last-index c-s))
          collect (funcall (udef-metadata-to-udef udef) i))))


;; TODO: method dispatch on UDEFs - it's possible on FIXNUM etc. as well!
;; then fix PRINT-UGLY?

;; TODO: box/unbox into (unsigned-byte X) specialized arrays and slots
;;       (Would that need a new widetag for each udef-tag?)

;; TODO: optimizing, especially the tags -- check all 16bit at once


;; TODO: optionally a freelist
;; TODO: integration in rucksack etc.
