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


;; Simple case:
;;
;;   ┌slots┐
;;   │  0  │──────────────>┌─slot-a─┐
;;   │  1  │──>┌─slot-b─┐  │  a[0]  │
;;   │ ... │   │  b[0]  │  │  a[1]  │
;;   └─────┘   │  b[1]  │  │  ...   │
;;             │  ...   │  │ a[n-1] │
;;             │ b[n-1] │  └────────┘
;;             └────────┘
;;
;; 2-level:
;;   ┌slots┐
;;   │  0  │──>┌──batch──┐
;;   │  1  │   │ [ 0- n] │─>┌─slot-a─┐
;;   │ ... │   │ [ n-2n] │  │  a[0]  │
;;   └─────┘   │ [2n-3n] │  │  a[1]  │
;;             │ [ ... ] │  │  ...   │
;;             └─────────┘  │ a[n-1] │
;;                          └────────┘
;;
;; The vector types are as narrow as possible.
;; This way memory usage is reduced (eg with (UNSIGNED-BYTE 16)).

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

;; Sizing heuristics for parallel allocation in threads
(defun default-batch-size (guess)
  (expt 2 (ceiling (log guess 2) 4/3)))

;; for ATOMIC-INCF to work, this must be a structure.
;; The slot names (rather their initargs) might collide with ones our users choose, sadly...
(defstruct (udef-c-s-metadata
             (:conc-name cs-meta-)
             ;(:include udef-metadata)
             )
  (udef            nil :type symbol     :read-only t)
  (data-var        nil :type symbol     :read-only t)
  (next              0 :type sb-vm:word)
  (allocated         0 :type sb-vm:word)
  (as-alist        nil :type symbol     :read-only t)
  (batch-size      nil :type (or null
                                 (integer 1 1000000000))
                       :read-only t)
  (lock            (sb-thread:make-mutex :name "c-s-upper-lock")
                       :type sb-thread:mutex
                       :read-only t)
  (slot-defs       nil :type vector     :read-only t)
  (slots-vec-sym   nil :type symbol :read-only t))

(defstruct (udef-c-s-slot
             (:type list) ; easier to serialize in macro expansion
             (:conc-name cs-slot-))
  ;; must be first slot!!
  (evaled-init   nil :type t                               :read-only t)
  (index         0   :type fixnum                          :read-only t)
  (slot-name     nil :type symbol                          :read-only t)
  (accessor-sym  nil :type symbol                          :read-only t)
  ;; User-supplied data
  (orig-type     nil :type t                               :read-only t)
  (initarg       nil :type t                               :read-only t)
  ;; Internally derived
  (trslt-type    nil :type t                               :read-only t)
  ;; vectors can be serialized
  (array-dim?    nil :type (or null sb-int:index)          :read-only t)
  (array-el-type nil :type t                               :read-only t)
  (udef          nil :type symbol                          :read-only t)
  (form          nil :type list                            :read-only t))


(defun cs-slot-is-nullable-udef? (slot)
  "Returns the UDEF structure or NIL."
  ;; TODO store & check nil-as-minus-1?
  (when (cs-slot-udef slot)
    (nth-value 1
               (get-udef-metadata-from-symbol
                 (cs-slot-orig-type slot)))))

(defun get-udef-metadata-from-symbol (sym &optional c-s-req)
  (when (symbolp sym)
    (let ((v-c-s (get sym 'column-struct-data))
          (udef  (get sym 'udef-metadata)))
      (when (and c-s-req
                 (not v-c-s))
        (error "~s is not a column-structure type." sym))
      (values v-c-s
              udef))))

;(declaim (inline get-new-id-range))
(defun get-new-id-range (obj &optional (count 1))
  (sb-ext:atomic-incf (cs-meta-next obj)
                      count))

(defun column-struct-reset (obj)
  "Soft-resets OBJ, ie. sets the last used index to 0."
  (if (symbolp obj)
      (column-struct-reset
        (get-udef-metadata-from-symbol obj t))
      (setf (cs-meta-next obj)
            0)))

(declaim (ftype (function (t) (sb-int:index)) column-struct-size))
(defun column-struct-size (obj)
  "Returns the allocated length (not the number of elements used)"
  (if (symbolp obj)
      (column-struct-size
        (get-udef-metadata-from-symbol obj t))
      (cs-meta-allocated obj)))


(defun column-struct-last-index (obj)
  "Returns the last used index;
  doesn't care about fragmentation with batched allocation."
  (if (symbolp obj)
      (column-struct-last-index
        (get-udef-metadata-from-symbol obj t))
      (cs-meta-next obj)))


(defun column-struct-get-struct (obj &key (output :alist))
  "Returns the data of OBJ as an alist."
  (declare (ignore output))
  (multiple-value-bind (type idx) (udef-inttype-type-of obj)
    ;; Optimizations possible
    (declare (ignore idx))
    (assert type)
    (let ((c-s (get-udef-metadata-from-symbol type)))
      (funcall (cs-meta-as-alist c-s)
               obj))))

(defun reduce-element-counts (vec new)
  (loop for slot across vec
        for i upfrom 0
        do (setf (aref vec i)
                 (adjust-array slot (list new)
                               :element-type (array-element-type slot)))))

(defun column-struct-resize (c-s new-size &key force-smaller)
  "Resizes to (at least) NEW-SIZE.
  Calls concurrency-unsafe ADJUST-ARRAY for a single-level column-struct,
  or adds/removes batches for a two-level column-struct."
  (sb-thread:with-mutex ((cs-meta-lock c-s))
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
           (setf (cs-meta-allocated c-s)
                 new-size)
           (sb-vm:%write-barrier)
           (reduce-element-counts (symbol-value (cs-meta-slots-vec-sym c-s))
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
         ;(format t "--------------------- for ~d~%" new-vec-size)
         ;(room t)
         (loop for slot-def across (cs-meta-slot-defs c-s)
               for slot-vec across (symbol-value (cs-meta-slots-vec-sym c-s))
               for slot-nr = (cs-slot-index slot-def)
               for orig-type = (cs-slot-orig-type slot-def)
               for slot-type = (or (cs-slot-trslt-type slot-def)
                                   (cs-slot-array-el-type slot-def)
                                   orig-type
                                   (error "No type"))
               for elem-per-slot = (or (cs-slot-array-dim? slot-def)
                                       1)
               for init-val = (cs-slot-evaled-init slot-def)
               for new-elements = (- new-vec-size (array-dimension slot-vec 0))
               ;do (format t "resize ~s: got ~s, ~s~%"
               ;           (cs-slot-slot-name slot-def)
               ;           (cs-slot-trslt-type slot-def)
               ;           slot-type)
               do (setf (aref (symbol-value (cs-meta-slots-vec-sym c-s))
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
                                                                  :initial-element init-val
                                                                  )))
                                   (data (concatenate 'list slot-vec new)))
                              (make-array new-vec-size
                                          :element-type (array-element-type slot-vec)
                                          :initial-contents data))
                            (let* ((new (make-list (* elem-per-slot new-elements)
                                                   :initial-element init-val
                                                   ))
                                   (data (concatenate 'list slot-vec new)))
                              (make-array (* elem-per-slot
                                             new-vec-size)
                                          :element-type slot-type
                                          :initial-contents data)))))
         (sb-vm:%write-barrier)
         (setf (cs-meta-allocated c-s)
               new-size)))))
  (column-struct-size c-s))

(defun column-struct-clear (obj &key new-initial-size)
  "Hard-resets OBJ, ie. drops storage vectors."
  (if (symbolp obj)
      (column-struct-reset
        (get-udef-metadata-from-symbol obj t))
      (progn
        (column-struct-reset obj)
        (column-struct-resize obj 0 :force-smaller t)
        (when new-initial-size
          (column-struct-resize obj new-initial-size))
        new-initial-size)))


(defun return-batch-macro (col-struct macro-name data-var batched
                                      constructor arg-list args)
  (sb-int:with-unique-names (next left new-batch)
    `(defmacro ,macro-name ((fn-name &key batch-size new-batch-cb) &body body)
       ,(format nil "FN-NAME is a local constructor taking the same arguments as the constructor of
                ~s, with a locally reserved range of IDs so that threads operate
                on different cache lines." col-struct)
       (check-type fn-name symbol)
       (sb-ext:with-current-source-form (body)
         `(let ((,',next 0)
                (,',left 0))
            (labels ((,',new-batch ()
                       ;; TODO: try to exactly fill up, so that other threads
                       ;; or the next calls have clean batches just for themselves?
                       (setf ,',left (or ,batch-size ,',batched))
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
            (get-udef-metadata-from-symbol element-type)
          (declare (ignore c-s))
          (list element-type
                dimension
                element-udef))))))

(defun get-slot-def (struct-name index input)
  (multiple-value-bind (name init user-type) (parse-slot input)
    (multiple-value-bind (c-s meta) (get-udef-metadata-from-symbol user-type)
      (declare (ignore c-s))
      (destructuring-bind (&optional arr-element-type arr-len arr-udef)
          (is-simple-1dim-array user-type)
        (declare (ignore arr-udef))
        (make-udef-c-s-slot
          :index index
          :slot-name name
          :initarg init
          :form input
          :orig-type user-type
          ;; Must be evaluated
          :evaled-init  `(let ((init ,init))
                           ,(if meta
                                `(sb-ext:with-current-source-form (',input)
                                   ;; use minus-1 value instead
                                   (funcall (udef-metadata-store-udef
                                              (nth-value 1 (get-udef-metadata-from-symbol ',user-type)))
                                            init))
                                'init))
          ;;
          :accessor-sym (sb-int:symbolicate struct-name "-" name)
          ;; TODO: array of UDEF, (or meta arr-udef)
          :udef (if meta t)
          :trslt-type (when meta
                          ;; Is a slot containing a UDEF,
                          ;; so use (UNSIGNED-BYTE x) instead
                        `(unsigned-byte ,(udef-metadata-max-bits meta)))
          :array-dim? arr-len
          :array-el-type arr-element-type
          )))))

(defun handle-c-s-slots (struct-name slot-input)
  ;; This ensures that we get #:SLOT-1, #:SLOT-2, for each column-structure,
  ;; so doesn't pollute the keyword package for the initargs as much
  ;; both &optional and &key
  (loop with new-val = (gensym "NEW-VAL")
        for s-i in slot-input
        for i upfrom 0
        for slot = (get-slot-def struct-name i s-i)
        for slot-type-nullable = (if (cs-slot-is-nullable-udef? slot)
                                     `(or null ,(cs-slot-orig-type slot))
                                     (cs-slot-orig-type slot))
        for slot-name = (cs-slot-slot-name slot)
        for acc = (cs-slot-accessor-sym slot)
        ;;
        collect slot into slots
        collect (sb-int:gensymify* slot-name :-INIT) into tmp-names
        ;;
        ;; This initarg is user-visible, use supplied value
        collect (list slot-name
                      (cs-slot-initarg slot)) into maker-args
        collect `(check-type ,slot-name
                             ,slot-type-nullable) into maker-checks
        ;;
        collect `(ftype (function (,struct-name) (values ,slot-type-nullable)) ,acc)
        into decl
        ; (inline ,e-accessor)
        collect `(defun ,acc (id)
                   ;; TODO: check-type
                   (declare (optimize (speed 3) (safety 1) (debug 1))
                            (type ,struct-name id))
                   (with-c-s-slots (,struct-name id) (,slot-name)
                     ;; Without the VALUES we get
                     ;;   note: Type assertion too complex to check efficiently:
                     ;;     (VALUES SB-INT:UDEF-INTTYPE &REST T).
                     ;;   It allows an unknown number of values, consider using
                     ;;     (VALUES SB-INT:UDEF-INTTYPE &OPTIONAL).
                     (values ,slot-name)))
        into code
        collect `(ftype (function (,slot-type-nullable ,struct-name) ,slot-type-nullable)
                        (setf ,acc))
        ;#+(or)(inline (setf ,e-accessor))
        into decl
        collect `(defun (setf ,acc) (,new-val id)
                   ;; TODO: check-type
                   (declare (optimize (speed 3) (safety 1) (debug 1))
                            (type ,struct-name id)
                            (type ,slot-type-nullable ,new-val))
                   (with-c-s-slots (,struct-name id) (,slot-name)
                     (values (setf ,slot-name ,new-val))))
        into code
        finally (return (values slots
                                code decl
                                maker-args maker-checks
                                tmp-names))))


(declaim (inline ref-udef-vec
                 (setf ref-udef-vec)))

(defun ref-udef-vec (vec index to-vec-repr from-vec-repr)
  (declare (ignore to-vec-repr))
  (funcall from-vec-repr
           (aref vec index)))

(defun (setf ref-udef-vec) (new vec index to-vec-repr from-vec-repr)
  (declare (ignore from-vec-repr))
  (setf (aref vec index)
        (funcall to-vec-repr new))
  new)

;; Hook into CL:WITH-SLOTS?
;; sb-mop:slot-value-using-class?
(defmacro with-c-s-slots ((udef-name index) names &body body)
  "Installs symbol-macros that access NAMES from UDEF-NAME
  at INDEX (which can resp. should be a UDEF).
  NAMES can be a list of slot names or a list of (SYM SLOT)."
  (multiple-value-bind (c-s def) (get-udef-metadata-from-symbol udef-name t)
    (sb-int:with-unique-names (val iidx idx batch)
      (loop for entry in names
            for e-list = (sb-int:ensure-list entry)
            for var-name = (first e-list)
            for slot-name = (or (second e-list) var-name)
            for slot-def = (or (find slot-name
                                       (cs-meta-slot-defs c-s)
                                       :test #'eq
                                       :key #'cs-slot-slot-name)
                               (error "Invalid slot name ~s" slot-name))
            for slot-vec-name = (sb-int:gensymify* udef-name :- var-name :-vec)
            for data-vec = (sb-int:gensymify* udef-name :- var-name :-data-vec)
            for slot-type = (cs-slot-orig-type slot-def)
            for stores-udef-ub = (cs-slot-trslt-type slot-def)
            for arr-el-type = (cs-slot-array-el-type slot-def)
            for elem-count = (cs-slot-array-dim? slot-def)
            for vec-type = (cond
                             (stores-udef-ub
                               `(simple-array ,stores-udef-ub (*)))
                             (arr-el-type
                               `(simple-array ,arr-el-type (*)))
                             ((not elem-count)
                               `(simple-array ,slot-type (*)))
                             ((eq t (upgraded-array-element-type arr-el-type))
                               `simple-vector)
                             (t
                               `(simple-array ,slot-type (*))))
            for stores-a-udef = (nth-value 1 (get-udef-metadata-from-symbol slot-type))
            for scaled-idx = (sb-int:gensymify* slot-name "-*-" (prin1-to-string elem-count))
            ;do (format t "~s:~% orig ~s,~% translated ~s,~% arr-el ~s,~% count ~s,~% gives ~s~%"
            ;           slot-name slot-type stores-udef-ub arr-el-type elem-count vec-type)
            ;; TODO: calculate multiples only once
            collect `(,scaled-idx (* ,idx ,(or elem-count 1))) into lets
            collect `(,slot-vec-name (aref ,(cs-meta-slots-vec-sym c-s) ,
                                           (cs-slot-index slot-def))) into lets
            collect `(,data-vec ,(if (cs-meta-batch-size c-s)
                                     `(aref ,slot-vec-name ,batch)
                                     slot-vec-name)) into lets
            collect `(type ,vec-type ,data-vec) into decls
            collect (if elem-count
                        `(,var-name (subseq ,data-vec
                                            ,scaled-idx
                                            (+ ,scaled-idx ,elem-count)))
                        `(,var-name ,(if stores-a-udef
                                         `(ref-udef-vec ,data-vec ,scaled-idx
                                                        (symbol-function ',(udef-metadata-store-udef stores-a-udef))
                                                        (symbol-function ',(udef-metadata-retr-udef  stores-a-udef)))
                                         ;; Not SVREF, these are SIMPLE-ARRAYs but not SIMPLE-VECTORs
                                         `(aref ,data-vec ,scaled-idx))))
            into syms
            finally (return
                      (let ((inner `(symbol-macrolet ,syms ,@body)))
                        `(let* ((,iidx ,index)
                                (,val (cond
                                        ((numberp ,iidx)
                                          ,iidx)
                                        ((,(udef-metadata-type-p def) ,iidx)
                                          (,(udef-metadata-store-udef def) ,iidx))
                                        (t (error "wrong input ~s, expect ~s udef"
                                                  ,iidx ',(cs-meta-udef c-s))))))
                           (unless (typep ,val '(unsigned-byte ,(udef-metadata-max-bits def)))
                             (error "Bad index value ~s, wanted a ~s for ~s"
                                    ,val
                                    '(unsigned-byte ,(udef-metadata-max-bits def))
                                                  ',(cs-meta-udef c-s)))
                           ,(if (cs-meta-batch-size c-s)
                                `(multiple-value-bind (,batch ,idx)
                                     (floor ,val ,(cs-meta-batch-size c-s))
                                   (let* ,lets
                                     (declare ,@ decls)
                                     ,inner))
                                `(let ((,idx ,val))
                                   (let* ,lets
                                     (declare ,@ decls)
                                     ,inner))))))))))


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
                 (with-instance-binding-sym (option :with-instance-binding nil))
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
                 (slots-vec-sym  (option :slots-vec-sym (sb-int:gensymify* struct-name :-slots-vec)))
                 ;;
                 (with-batch-macro (option :with-batch-allocation-name nil))
                 (constructor-name (option :constructor
                                           (intern (format nil "~a~a" :make- struct-name)
                                                   (symbol-package struct-name))))
                 ;;
                 (data-var (option :data-var (sb-int:gensymify* :data- struct-name)))
                 (as-alist-sym (sb-int:gensymify* struct-name :-as-alist)))
            (multiple-value-bind (slot-defs code decl
                                            maker-arg-list maker-checks
                                            tmp-names)
                (handle-c-s-slots struct-name slots)
              (when (zerop (length slot-defs))
                (error "Need at least one slot in ~s" struct-name))
              (let ((slot-names (mapcar #'cs-slot-slot-name slot-defs))
                    (tmp (gensym)))
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
                     ;;
                     ;; The elements of the slot-vectors have different types.
                     (declaim (type (simple-array t (*)) ,slots-vec-sym)
                              (sb-ext:global ,slots-vec-sym))
                     (setf ,slots-vec-sym
                           (make-array ,(length slot-defs)
                                       :element-type '(simple-array t (*))
                                       :initial-contents (loop repeat ,(length slot-defs)
                                                               collect #())))
                     (declaim (sb-ext:always-bound ,slots-vec-sym))
                     ;;
                     (declaim (type udef-c-s-metadata ,data-var)
                              (sb-ext:global ,data-var))
                     ;; TODO: loses value upon reload, keep old contents?
                     (setf ,data-var
                           (make-udef-c-s-metadata
                             :udef          ',struct-name
                             :data-var      ',data-var
                             :batch-size    ,batch-size
                             ;; initarg gets evaluated!
                             :slot-defs     (vector ,@ (mapcar (lambda (slot)
                                                               `(list* ,(first slot)
                                                                       (quote ,(rest slot))))
                                                             slot-defs))
                             :slots-vec-sym ',slots-vec-sym
                             :as-alist      ',as-alist-sym))
                     (setf (get ',struct-name 'column-struct-data)
                           ,data-var)
                     (declaim (sb-ext:always-bound ,data-var))
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
                               do (column-struct-resize ,where
                                                        (max (round (* (sqrt 2) ,old-size))
                                                             (+ ,old-size
                                                                ,(or batch-size 50))))))
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
                       (let ((,idx (get-new-id-range ,data-var)))
                         (,base-constructor ,data-var ,idx ,@ slot-names)))
                     ;;
                     (declaim ,@ decl)
                     ,@ code
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
                                            data-var batch-size base-constructor
                                            maker-arg-list slot-names))
                     ;;
                     ,(when with-instance-binding-sym
                        `(defmacro ,with-instance-binding-sym ((udef-type storage-slot)
                                                               &body body)
                           "Binds local column-struct-instance for UDEF-TYPE,
                           using STORAGE-SLOT for keeping the instance data."
                           :TODO))
                     ;; BROKEN: There is no class named X.
                     #+(or)
                     (defmethod sb-c::describe-object :after ((obj ,struct-name) stream)
                       (with-c-s-slots (,struct-name ,idx) slot-names
                         ,@(loop for name in slot-names
                                 collect `(format stream "~&  ~A = ~A~%" ',name ,name)))))
                   ;;
                   (column-struct-reset ,data-var)
                   (column-struct-resize ,data-var ,initial-size)
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
    (multiple-value-bind (c-s udef) (get-udef-metadata-from-symbol obj)
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


(defun map-c-s-range (fn col-struct)
  "Runs MAPCAR over all defined values for COL-STRUCT
  calling FN with the UDEF."
  (multiple-value-bind (c-s udef) (get-udef-metadata-from-symbol col-struct t)
    (loop for i upfrom 0
          ;; Not a BELOW, because the size may change during iteration
          while (<= i (column-struct-last-index c-s))
          for u = (funcall (udef-metadata-to-udef udef) i)
          collect (funcall fn u))))

(defun c-s-values (col-struct)
  "Returns a list of all defined COL-STRUCT values."
  (multiple-value-bind (c-s udef) (get-udef-metadata-from-symbol col-struct t)
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
