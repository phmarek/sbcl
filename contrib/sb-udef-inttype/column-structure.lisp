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


;; Sizing heuristics for parallel allocation in threads
(defun default-batch-size (guess)
  (expt 2 (ceiling (log guess 2) 4/3)))

(defstruct (udef-c-s-metadata
             (:conc-name cs-meta-))
  (udef            nil :type symbol          :read-only t)
  (next              0 :type sb-vm:word)
  (allocated         0 :type sb-vm:word)
  (as-alist        nil :type symbol          :read-only t)
  (batch-size      nil :type (or null
                                 ;; should actually start at 1000 or so,
                                 ;; but race tests run with small increments
                                 (integer 3 1000000000))
                   ;; Allow modification during test
                       :read-only nil)
  (lock            (sb-thread:make-mutex :name "c-s-upper-lock")
                       :type sb-thread:mutex
                       :read-only t)
  (slots           nil :type vector          :read-only t)
  ;; Slot defining the length for variable-length slots
  (var-len-slot    nil :type t               :read-only t)
  (vec-slot-count    0 :type fixnum          :read-only t)
  (has-index-slot  nil :type (member nil t ) :read-only t)
  (data-vec        #() :type simple-vector   :read-only t))


(defun find-slot-by-name (c-s slot-name)
  (find slot-name
        (if (consp c-s)
            c-s
            (cs-meta-slots (if (symbolp c-s)
                               (get-cs-metadata-from-symbol c-s)
                               c-s)))
        :test #'eq
        :key #'cs-s-slot-name))

;; -----

(defclass cs-slot-common ()
  ((slot-name     :initform nil :initarg :slot-name    :type symbol        :reader cs-s-slot-name    )
   (accessor-sym  :initform nil :initarg :accessor-sym :type symbol        :reader cs-s-accessor-sym )
   (orig-type     :initform nil :initarg :orig-type    :type t             :reader cs-s-orig-type    )
   ;; User-supplied data
   (init-fn       :initform nil :initarg :init-fn      :type t             :reader cs-s-init-fn      )
   (init-form     :initform nil :initarg :init-form    :type t             :reader cs-s-init-form    )))

(defclass cs-mixin-vector-storage-slot ()
  ((index         :initform 0   :initarg :index        :type fixnum        :reader cs-s-index        )))

(defclass cs-mixin-array ()
  ((array-el-type    :initform nil :initarg :array-el-type :type t :reader cs-s-array-el-type)
   (single-init-form :initform nil :initarg :s-init-form   :type t :reader cs-s-single-init-form)
   (single-init-fn   :initform nil :initarg :s-init-fn     :type t :reader cs-s-single-init-fn)))

(defclass cs-mixin-fixed-size-array (cs-mixin-array cs-mixin-vector-storage-slot cs-slot-common)
  ;; Fixed-size vectors can be serialized into a (multiple-sized) vector
  ((fixed-len      :initform 0   :initarg :fixed-len       :type sb-int:index :reader cs-s-array-fixed-len)))

(defclass cs-mixin-variable-sized-array (cs-mixin-array cs-mixin-vector-storage-slot cs-slot-common)
   ;; variable for variable length
  ((len-var       :initform nil :initarg :array-len-var :type t             :reader  cs-s-array-len-var)))

(defclass cs-mixin-udef ()
  ((udef-sym      :initform nil :initarg :udef-sym      :type symbol        :reader cs-s-udef-sym)
   ;; TODO drop?
   (u-slot-type   :initform nil :initarg :u-slot-type   :type list          :reader cs-s-u-slot-type)
   ))

(defclass cs-mixin-immediate ()
  ((start         :initform  -1 :initarg :start         :type fixnum             :reader cs-s-bit-start)
   (count         :initform   0 :initarg :count         :type fixnum             :reader cs-s-bit-count)
   ))

(defclass cs-mixin-udef-array (cs-mixin-array cs-mixin-udef)
  ())

(defclass cs-mixin-unsigned-byte ()
  ())

;; -----

(defclass cs-any-type-slot (cs-slot-common cs-mixin-vector-storage-slot)
  ())

(defclass cs-udef-ref-slot (cs-mixin-udef cs-slot-common cs-mixin-vector-storage-slot)
  ())

(defclass cs-s-udef-fix-array (cs-mixin-udef-array cs-mixin-fixed-size-array)
  ())

(defclass cs-s-udef-var-array (cs-mixin-udef-array cs-mixin-variable-sized-array)
  ())

;; actually for type T?
(defclass cs-s-ub-fix-array (cs-mixin-unsigned-byte cs-mixin-fixed-size-array)
  ())

;; actually for type T?
(defclass cs-s-ub-var-array (cs-mixin-unsigned-byte cs-mixin-variable-sized-array )
  ())

(defclass cs-s-imm-udef (cs-mixin-udef cs-mixin-immediate cs-slot-common)
  ())

(defclass cs-s-imm-ub (cs-mixin-immediate cs-slot-common)
  ())

(defmethod cs-s-slot-name ((obj (eql nil)))
  nil)

(defmethod print-object ((obj cs-slot-common) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~@[~d = ~]~s"
            (when (typep obj 'cs-mixin-vector-storage-slot)
              (cs-s-index obj))
            (cs-s-slot-name obj))))

;; ------------------------------------------------------------

(defvar *slot-nr* nil
  "Used to number the slots during parsing")
(defvar *bit-index* nil
  "Used to count used immediate bits during parsing")
(defvar *current-cs-sym* nil
  "The symbol we're defining a C-S on.")
(defvar *current-cs-size* nil
  "The number of bits for the current C-S.")

;; ------------------------------------------------------------


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


(defmacro load-time-slot-vector (sym)
  `(load-time-value (cs-meta-data-vec
                      (get-cs-metadata-from-symbol ,sym))))

(defmacro load-time-col-struct (sym)
  `(load-time-value (get-cs-metadata-from-symbol ,sym)))

(defmacro load-time-udef-struct (sym)
  `(load-time-value
     (get-udef-metadata-from-symbol ,sym)))


;; ------------------------------------------------------------

(defgeneric cs-s-array-length-spec (slot)
  (:method ((slot cs-mixin-fixed-size-array))
    (cs-s-array-fixed-len slot))
  (:method ((slot cs-mixin-variable-sized-array))
    '*))

(defgeneric array-slot-index-multiplier (slot)
  (:method ((slot cs-mixin-fixed-size-array))
    (cs-s-array-fixed-len slot))
  (:method (slot)
    1))


(defmethod cs-s-ldb ((slot cs-mixin-immediate))
  (with-slots (count start) slot
    (byte count start)))


(defgeneric cs-s-storage-type (item)
  (:documentation
   "Returns the _storage_ type for ITEM.")
  (:method ((slot (eql nil)))
    (error "No storage for NIL"))
  (:method ((slot cs-slot-common))
    (cs-s-orig-type slot))
  (:method ((slot cs-mixin-udef))
    (cs-s-u-slot-type slot))
  (:method ((u (eql :current)))
    `(unsigned-byte ,*current-cs-size*))
  (:method ((u udef-metadata))
    `(unsigned-byte ,(udef-metadata-max-bits u)))
  (:method ((slot cs-mixin-immediate))
    (error "no storage for immediate"))
  (:method ((slot cs-mixin-array))
    (cs-s-array-el-type slot)))

(defgeneric cs-s-input-type (item)
  (:documentation
   "Returns the _input_ type for ITEM.")
  (:method :around ((slot cs-mixin-array))
    `(array ,(cs-s-array-el-type slot)
            (, (cs-s-array-length-spec slot))))
  (:method ((slot cs-mixin-array))
    (cs-s-array-el-type slot))
  (:method ((slot cs-any-type-slot))
    (cs-s-orig-type slot))
  (:method ((slot cs-mixin-immediate))
    `(unsigned-byte ,(cs-s-bit-count slot)))
  (:method ((slot cs-mixin-udef))
    `(or ,(cs-s-udef-sym slot)
         null)))

(defgeneric single-initial-element (slot)
  (:method ((slot cs-slot-common))
    (funcall (cs-s-init-fn slot)))
  (:method ((slot cs-mixin-udef))
    (let ((value (funcall (cs-s-init-fn slot))))
      ;; Translate to right bitmask
      (destructuring-bind (to from) (wrappers-for-value-translation slot)
        (funcall to value t)))
  (:method ((slot cs-mixin-array))
   (funcall (cs-s-single-init-fn slot))))



;(declaim (inline get-new-id-range))
(defun get-new-id-range (obj &optional count)
  "Returns the _old_ value, before incrementing"
  (let* ((count (or count 1)))
    (cond
       ;; Single-level allocation or one element only.
       ;; Maybe needs resizing; the constructor will do that.
      ((or (= count 1)
           (not (cs-meta-batch-size obj)))
       (sb-ext:atomic-incf (cs-meta-next obj) count))
      ;; Check allocation constraint
      ((< (cs-meta-batch-size obj) count)
       (error "Allocation ~d larger than batch size ~s for ~s"
              count
              (cs-meta-batch-size obj)
              (cs-meta-udef obj)))
      ;; Find a suitable position.
      (t
       ;; Straight allocation would get split over two batches?
       ;; We can't allow that.
       (dotimes (i 15)
         (let* ((cur (cs-meta-next obj))
                (position-within-batch (mod cur (cs-meta-batch-size obj)))
                (next-pos (+ position-within-batch count)))
           (if (< next-pos (cs-meta-batch-size obj))
               ;; Get current position
               (let ((chg (sb-ext:compare-and-swap (cs-meta-next obj)
                                                   cur
                                                   next-pos)))
                 (if (= chg cur)
                     (return-from get-new-id-range cur)
                     ;; Else raced, retry.
                     nil))
               ;; Doesn't fit into begun batch.
               (let* ((next-batch (+ cur
                                     (- position-within-batch)
                                     (cs-meta-batch-size obj)))
                      (allocation-end (+ next-batch count)))
                 ;; Try to get that position
                 (let ((chg (sb-ext:compare-and-swap (cs-meta-next obj)
                                                     cur
                                                     allocation-end)))
                   (when (= chg cur)
                     (return-from get-new-id-range next-batch)))))))
       (error "can't get new allocation for ~s even after a few retries. broken logic?"
              (cs-meta-udef obj))))))

(define-compiler-macro get-new-id-range (&whole whole obj &optional count)
  (if (or (and (constantp count)
               (numberp count)
               (= count 1))
          (null count))
      `(sb-ext:atomic-incf (cs-meta-next ,obj))
      whole))

;; ------------------------------------------------------------

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

(defmethod column-struct-resize ((c-s symbol) new-size &key force-smaller)
  (column-struct-resize (get-cs-metadata-from-symbol c-s t)
                        new-size
                        :force-smaller force-smaller))

(defmethod column-struct-resize ((c-s udef-c-s-metadata) new-size &key force-smaller)
  "Resizes to (at least) NEW-SIZE.
  Calls concurrency-unsafe ADJUST-ARRAY for a single-level column-struct,
  or adds/removes batches for a two-level column-struct."
  ;(declare (optimize (sb-c:instrument-consing 0)))
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
                 when (typep slot-def 'cs-mixin-vector-storage-slot)
                 do (let* ((slot-nr (cs-s-index slot-def))
                           (slot-vec (aref (cs-meta-data-vec c-s) slot-nr))
                           (slot-type (cs-s-storage-type slot-def))
                           (elem-per-slot (array-slot-index-multiplier slot-def))
                           (new-elements (- new-vec-size (array-dimension slot-vec 0))))
                      (setf (aref (cs-meta-data-vec c-s)
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
                                              :initial-contents data))))))
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

;; ------------------------------------------------------------

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
                     ;; Index gets calculated
                     (,fn-name (&key ,@', (rest arg-list))
                       (let* ((id ,',next))
                         (when (<= ,',left 0)
                           (,',new-batch)
                           (setf id ,',next))
                         (incf ,',next)
                         (decf ,',left)
                         (,',constructor id ,@', args))))
              (locally
                ,@ body)))))))

;; ------------------------------------------------------------

(defun parse-slot (input)
  (declare (sb-ext:muffle-conditions style-warning))
  (destructuring-bind (name &optional init &key (type t) (allocation :default))
      (sb-int:ensure-list input)
    (values name init type allocation)))

(defun is-simple-1dim-array (type)
  (ignore-errors
    ;; restricted to a subset of possible array-types
    (destructuring-bind (kind element-type (dimension)) type
      (when (member kind '(array simple-array))
        (multiple-value-bind (c-s element-udef)
            (get-cs-metadata-from-symbol element-type)
          (declare (ignore c-s))
          (list element-type
                dimension
                element-udef))))))

(defmethod wrappers-for-value-translation (slot)
  (list 'identity 'identity))

(defmethod wrappers-for-value-translation ((slot cs-mixin-udef))
  (wrappers-for-value-translation
    (get-udef-metadata-from-symbol
      (cs-s-udef-sym slot))))

(defmethod wrappers-for-value-translation ((udef sb-impl:udef-metadata))
  (list (sb-impl:udef-metadata-to-udef udef)
        (sb-impl:udef-metadata-from-udef udef)))


;; Getters --------------------------------------------------
;;
;; TODO: check-type
;; Without the VALUES we get
;;   note: Type assertion too complex to check efficiently:
;;     (VALUES SB-INT:UDEF-INTTYPE &REST T).
;;   It allows an unknown number of values, consider using
;;     (VALUES SB-INT:UDEF-INTTYPE &OPTIONAL).

(defmethod getter-and-setter-functions ((slot cs-mixin-immediate) cs)
  (declare (ignore cs))
  (destructuring-bind (getter setter) (wrappers-for-value-translation slot)
    (with-slots (accessor-sym slot-name) slot
      `((,accessor-sym (id)
           (declare (optimize (speed 3) (safety 1) (debug 1))
                    (type ,*current-cs-sym* id))
           (,getter (ldb ',(cs-s-ldb slot)
                         (sb-kernel:get-lisp-obj-address id))))
        ((setf ,accessor-sym) (new place)
           (declare (optimize (speed 3) (safety 1) (debug 1))
                    (type ,*current-cs-sym* place))
           ;; TODO: No setter function for immediate slots -
           ;; they're used to _address_ data and so can't be changed afterwards??!
           (let ((raw (sb-kernel:get-lisp-obj-address place)))
             (setf (ldb ',(cs-s-ldb slot) raw)
                   (,setter new))
             (setf place
                   (sb-kernel:%make-lisp-obj raw)))
           new)))))


(defun setup-slot-vector-addressing (c-s slot input vec-name vec-index-sym body-form)
  (let ((input-udef-def  (get-udef-metadata-from-symbol
                           (cs-meta-udef c-s))))
    (sb-int:with-unique-names (iidx input-index slots-vec batch-var batch-idx inner-idx)
      `(let* ((,iidx ,input)
              (,input-index (cond ((,(udef-metadata-type-p input-udef-def) ,iidx)
                                   (,(cs-s-accessor-sym
                                       (aref (cs-meta-slots c-s)
                                             0))
                                     ,iidx))
                                  (t
                                   (error "wrong input ~s, expect ~s udef"
                                          ,iidx ',(cs-meta-udef c-s))))))
         (multiple-value-bind (,batch-idx ,inner-idx)
             ;; TODO: batch-size changeable during runtime?
             ,(if (cs-meta-batch-size c-s)
                  `(floor ,input-index ,(cs-meta-batch-size c-s))
                  `(values nil ,input-index))
           (declare (ignorable ,batch-idx))
           (let* ((,slots-vec (load-time-slot-vector ',*current-cs-sym*))
                  (,batch-var (aref ,slots-vec ,(cs-s-index slot)))
                  ;; (the T ; ,element-vec-type TODO )
                  (,vec-name ,(if (cs-meta-batch-size c-s)
                                  `(aref ,batch-var ,batch-idx)
                                  `,batch-var))
                  (,vec-index-sym (* ,inner-idx
                                     ,(array-slot-index-multiplier slot))))
             (declare (ignorable ,vec-name))
             ;(declare (type (simple-array ,element-vec-type (*))))
             , body-form))))))


(defmethod getter-and-setter-functions ((slot cs-mixin-array) cs)
  (with-slots (accessor-sym slot-name) slot
    (destructuring-bind (reader store) (wrappers-for-value-translation slot)
      `((,accessor-sym (id &key start end index (mode :displaced))
        (declare (optimize (speed 3) (safety 1) (debug 3))
                 (type ,*current-cs-sym* id))
        ,(setup-slot-vector-addressing
           (get-cs-metadata-from-symbol *current-cs-sym*)
           slot 'id 'vec 'idx
           `(let ((max-len ,(get-length-of-array-slot-form slot cs 'id)))
              (assert (typep index `(or null
                                        (integer 0 ,max-len))))
              (assert (typep start `(or null
                                       (integer 0 ,max-len))))
              (assert (typep end `(or null
                                     (integer ,(or start 0) ,max-len))))
              (if index
                  (values (,reader
                            (aref vec (+ idx index))))
                  (let* ((real-start (+ idx (or start 0)))
                         (real-end (+ idx (or end max-len)))
                         (len (- real-end real-start)))
                    (ecase mode
                      (:displaced
                       (values
                         ,(let ((backend-data `(make-array len
                                          :element-type ',(cs-s-storage-type slot)
                                          :displaced-to vec
                                          :displaced-index-offset real-start)))
                            (if (eq 'identity reader)
                                backend-data
                                ;; TODO: How to get changes reflected into the original
                                ;; if there's a translation, eg. for an UDEF??
                                `(make-array len
                                             :element-type ',(cs-s-array-el-type slot)
                                             :initial-contents
                                             (map 'list #' ,reader ,backend-data))))))
                      (:subseq
                       (values
                         ,(let ((backend-data  `(subseq vec real-start real-end)))
                            (if (eq 'identity reader)
                                backend-data
                                `(map `(array ,',(cs-s-array-el-type slot) ,len)
                                     #' ,reader ,backend-data)))))))))))
      ((setf ,accessor-sym) (new id &key start end index)
        (declare (optimize (speed 3) (safety 1) (debug 3))
                 (type ,*current-cs-sym* id))
        ,(setup-slot-vector-addressing
           (get-cs-metadata-from-symbol *current-cs-sym*)
           slot 'id 'vec 'idx
           `(let ((max-len ,(get-length-of-array-slot-form slot cs 'id)))
              (assert (typep index `(or null
                                        (integer 0 ,max-len))))
              (assert (typep start `(or null
                                        (integer 0 ,max-len))))
              (assert (typep end `(or null
                                      (integer ,(or start 0) ,max-len))))
              (if index
                  (setf (aref vec (+ index idx))
                        (,store new))
                  (let* ((real-start (+ idx (or start 0)))
                         (real-end (+ idx (or end max-len)))
                         #+(or)
                         (len (- real-end real-start)))
                    (loop for el across new
                          for i upfrom real-start below real-end
                          do (setf (aref vec i)
                                   (,store el)))
                    #+(or) ;; TODO: with a MAP for translation faster than a loop?
                    (replace
                      (make-array len
                                  :element-type ',(cs-s-storage-type slot)
                                  :displaced-to vec
                                  :displaced-index-offset real-start)
                      new)
                    new)))))))))


(defmethod getter-and-setter-functions ((slot cs-mixin-vector-storage-slot) cs)
  (destructuring-bind (reader store) (wrappers-for-value-translation slot)
    (with-slots (accessor-sym slot-name) slot
      `((,accessor-sym (id)
          (declare (optimize (speed 3) (safety 1) (debug 3))
                   (type ,*current-cs-sym* id))
          ,(setup-slot-vector-addressing
             (get-cs-metadata-from-symbol *current-cs-sym*)
             slot 'id 'vec 'idx
             `(,reader (aref vec idx))))
        ((setf ,accessor-sym) (new id)
          (declare (optimize (speed 3) (safety 1) (debug 3))
                   (type ,*current-cs-sym* id))
          ,(setup-slot-vector-addressing
             (get-cs-metadata-from-symbol *current-cs-sym*)
             slot 'id 'vec 'idx
             `(setf (aref vec idx)
                    (,store new))))))))


(defmacro value-before-incf (place &optional (count 1))
  `(prog1
       ,place
     (incf ,place ,count)))

(defun slot-def-has-some-mixin? (slot-def mixin)
  (member (find-class mixin)
          (sb-mop:compute-class-precedence-list
            (find-class
              ;; MAKE-INSTANCE (QUOTE name)
              (second (second slot-def))))))

(defun get-slot-def (input)
  (multiple-value-bind (name init user-type allocation) (parse-slot input)
    (multiple-value-bind (c-s meta) (get-cs-metadata-from-symbol user-type)
      (declare (ignore c-s))
      (destructuring-bind (&optional arr-element-type arr-len arr-udef)
          (is-simple-1dim-array user-type)
        (multiple-value-bind (type+args init-fn-args)
            (cond
              ((eq allocation :immediate)
               (multiple-value-bind (bits)
                   (cond
                     (meta
                      (udef-metadata-max-bits meta))
                     ((and (consp user-type)
                           (eq (first user-type) 'unsigned-byte)
                           (numberp (second user-type)))
                      (second user-type))
                     ((eq user-type *current-cs-sym*)
                      (setf meta :current)
                      *current-cs-size*)
                     (t
                      (error ":ALLOCATION :IMMEDIATE only allowed for (UNSIGNED-BYTE x) or UDEF types")))
                   (values (list (if meta
                                     ''cs-s-imm-udef
                                     ''cs-s-imm-ub)
                                 :count bits
                                 :start `(value-before-incf *bit-index* ,bits)
                                 )
                           nil)))
              ((not (eq allocation :default))
               ;; TODO: allow individual slots to specify 1 or 2 levels?
               ;; Doesn't make that much sense, does it?
               (error "Bad :ALLOCATION"))
              (arr-element-type
               ;; TODO: doesn't work on arrays-of-arrays and similar
               (let ((var-len (and (symbolp arr-len)
                                   arr-len)))
                 (destructuring-bind (reader store) (wrappers-for-value-translation arr-udef)
                   (declare (ignore reader))
                   (multiple-value-bind (single-init vec-init)
                       (cond
                         ((and (consp init)
                               (eq (first init) 'make-array))
                          (destructuring-bind (m-a len &key initial-element initial-contents &allow-other-keys) init
                            (declare (ignore m-a len))
                            (when initial-contents
                              (error "C-S init for :initial-contents NIY, in ~s" *current-cs-sym*))
                            (values `(,store ,initial-element)
                                    `(error "not used?? 713")
                                    init)))
                         (t
                          (values `(,store ,init)
                                  `(make-array
                                     (list ,arr-len)
                                     :element-type '(or null ,arr-element-type)
                                     :initial-element ,init))))
                     (values `(,@(if var-len
                                     (list (if arr-udef
                                               ''cs-s-udef-var-array
                                               ''cs-s-ub-var-array)
                                           :array-len-var `',var-len)
                                     (list (if arr-udef
                                               ''cs-s-udef-fix-array
                                               ''cs-s-ub-fix-array)
                                           :fixed-len arr-len))
                                :init-form ',vec-init
                                :init-fn (lambda ,(when var-len
                                                    (list var-len))
                                           ,(when var-len
                                              `(declare (ignorable ,var-len)))
                                           ,vec-init)
                                :s-init-form ',single-init
                                :s-init-fn (lambda ()
                                             ,single-init)
                                :array-dim ,(when (numberp arr-len)
                                              arr-len)
                                :array-el-type ',arr-element-type)
                                (when var-len
                                  `(,var-len)))))))
              (meta
               ;; see below!
               (values `('cs-udef-ref-slot)))
              (t
               (values `('cs-any-type-slot))))
          ;;
          `(make-instance ,@ type+args
                          ;; for CS-S-UDEF-*-ARRAY or immediate slots
                          ,@ (when meta
                               `(:udef-sym ',user-type
                                 :u-slot-type ',(cs-s-storage-type meta)))
                          ,@ (when arr-udef
                               `(:udef-sym ',arr-element-type
                                 :u-slot-type ',(cs-s-storage-type arr-udef)))
                          ,@ (when (slot-def-has-some-mixin? (list* 'make-instance type+args)
                                                             'cs-mixin-vector-storage-slot)
                               `(:index (value-before-incf *slot-nr*)))
                          :orig-type    ',user-type
                          :slot-name    ',name
                          :accessor-sym ',(sb-int:symbolicate *current-cs-sym* "-" name)
                          :init-form    ',init
                          ;; default
                          :init-fn      (sb-ext:with-current-source-form (',input)
                                          (lambda , init-fn-args
                                            (declare (ignore ,@ init-fn-args))
                                            ,init))
                          :allow-other-keys t))))))

(defun check-var-len-slots (slots)
  ;; all slots with same variable length multiple
  (let ((var-len-args (remove nil
                              (map 'list
                                   (lambda (slot)
                                     (when (typep slot 'cs-mixin-variable-sized-array)
                                       (cs-s-array-len-var slot)))
                                   slots))))
    (when (> (length var-len-args) 1)
      (unless (and (apply #'eq var-len-args)
                   (find-slot-by-name (first var-len-args) slots))
        (error "All slots with variable length vectors must use the same symbol for the length,~
               and it must be defined as an slot (possibly with :ALLOCATION :IMMEDIATE).")))
    (first var-len-args)))

;; ------------------------------------------------------------

(defgeneric get-length-of-array-slot-form (slot cs udef)
  (:documentation
   "For SLOT being used in CS, return a form that specifies
   the length - an integer or a getter function for variable length.")
  (:method ((slot cs-mixin-variable-sized-array) cs udef)
      ;; Use accessor to read length
      `(,(cs-s-accessor-sym (cs-meta-var-len-slot cs)) ,udef))
  (:method ((slot cs-mixin-fixed-size-array) cs udef)
      (cs-s-array-fixed-len slot)))

(defmethod create-form-for_with-cs (cs udef-input name (slot cs-mixin-immediate) body)
  (let ((fn (sb-int:gensymify* (cs-s-slot-name slot)))
        (typep-fn (sb-impl:udef-metadata-type-p
                    (get-udef-metadata-from-symbol
                      (cs-meta-udef cs))))
        (slot-udef (get-udef-metadata-from-symbol (cs-s-udef-sym slot))))
    (declare (ignore slot-udef))
    `(flet
         ((,fn ()
            (cond
              ;; For correct udef extract
              ((,typep-fn ,udef-input)
               (ldb ,(cs-s-ldb slot)
                    (sb-kernel:get-lisp-obj-address ,udef-input)))
              ;; fetch from UNSIGNED-BYTE value
              ((typep ,udef-input ',(cs-s-storage-type cs))
               ;; Repair start position: in untagged integer further to the LSB
               (ldb ,(destructuring-bind (typ l s) (cs-s-ldb slot)
                       (list typ l (- s sb-impl::+udef-reserved-low-bits+)))
                    (sb-kernel:get-lisp-obj-address ,udef-input))))
            ((setf ,fn) (new)
                        (if (,typep-fn ,udef-input)
                            (setf (ldb ,(cs-s-ldb slot) ,udef-input)
                                  new)
                            (error "SETF of immediate slots only for UDEFs.")))))
       (symbol-macrolet ((,name () (,fn)))
         ,@ body))))

#+(or)
(defmethod create-form-for_with-cs (cs udef-input name (slot cs-s-imm-udef) body)
  (let ((to-storage (sb-int:gensymify* (cs-s-slot-name slot) :-store))
        (from-storage (sb-int:gensymify* (cs-s-slot-name slot) :-read))
        (sym (gensym)))
    (call-next-method cs udef-input sym slot
                      `(flet ( ,@(wrappers-for-value-translation slot to-storage from-storage)
                               (symbol-macrolet ((,name ,sym))
                                 ,@body))))))


;; Hook into CL:WITH-SLOTS?
;; sb-mop:slot-value-using-class?
(defmacro with-c-s-slots ((udef-name index) names &body body)
  "Installs symbol-macros that access NAMES from UDEF-NAME
  at INDEX (which can resp. should be a UDEF).
  NAMES can be a list of slot names or a list of (SYM SLOT)."
  (multiple-value-bind (c-s def) (get-cs-metadata-from-symbol udef-name t)
    (sb-int:with-unique-names (iidx val slots-vec batch-idx inner-idx c-s-var)
      `(let* ((,iidx ,index)
              (,val (cond
                      ((,(udef-metadata-type-p def) ,iidx)
                        (,(cs-s-accessor-sym
                            (aref (cs-meta-slots c-s)
                                  0))
                          ,iidx))
                      (t
                        (error "wrong input ~s, expect ~s udef"
                               ,iidx ',(cs-meta-udef c-s)))))
              (,c-s-var (load-time-col-struct ',udef-name))
              (,slots-vec (load-time-slot-vector ',udef-name)))
         (declare (ignorable ,c-s-var ,slots-vec))
         (unless (typep ,val ',(cs-s-storage-type def))
           (error "Bad index value ~s, wanted a ~s for ~s"
                  ,val
                  ',(cs-s-storage-type def)
                  ',(cs-meta-udef c-s)))
         (multiple-value-bind (,batch-idx ,inner-idx)
             ,(if (cs-meta-batch-size c-s)
                `(floor ,val (cs-meta-batch-size ,c-s-var))
                `(values nil ,val))
           (declare (ignorable ,batch-idx ,inner-idx))
           ,(loop with content = `(progn ,@ body)
                  ;; "inner to outer" means (reverse names),
                  ;; but it shouldn't make a difference
                  for entry in (reverse names)
                  for e-list = (sb-int:ensure-list entry)
                  for var-name = (first e-list)
                  for slot-name = (or (second e-list) var-name)
                  for slot = (or (find-slot-by-name c-s slot-name)
                                 (error "Invalid slot name ~s" slot-name))
                  for fn = (sb-int:gensymify* var-name :-fn)
                  ;for form = (create-form-for_with-cs c-s iidx
                  ;                                    var-name
                  ;                                    slot
                  ;                                    content)
                  for form = `(symbol-macrolet
                                  ;; Must access the original place because of immediate slots!
                                  ;; TODO: warn on side-effects?
                                  ((,var-name (,(cs-s-accessor-sym slot) ,index)))
                                , content)
                  do (setf content form)
                  finally (return form)))))))


;; ------------------------------------------------------------


(defun make-udef-with-immediate-slots-form (to-udef-sym slots)
  `(,to-udef-sym (logior
                   ,@ (loop for slot across slots
                            when (typep slot 'cs-mixin-immediate)
                            collect `(ash ,(cs-s-slot-name slot)
                                          ,(- (cs-s-bit-start slot)
                                              sb-impl::+udef-reserved-low-bits+))))))


(defmacro def-column-struct (name-and-options &rest slot-definitions)
  "Like DEFSTRUCT, but creates a (or, for :BATCHED, multiple) array(s) per slot,
  and the \"identity\" of an instance is actually the index
  in the arrays, returned as a user-defined integer type.

  Redefinition looses all old data (like with DEFSTRUCT,
  though for different reasons.).

  Available options:
  - :CONSTRUCTOR (defaults to MAKE-<name>)
  - :INDEX-BITS resp. :MAX-BITS
  - :INITIAL-SIZE to avoid initial reallocations
  - :BATCHED allocation size for a 2-level structure;
    pass T for a heuristic to derive a size from :INITIAL-SIZE
  - :WITH-BATCH-MACRO to provide a macro that does batch allocations,
    for use in heavy multi-threaded programs

  Advantages:
  - GC only sees a few big specialized vectors, so does much less work
  - smaller instance \"pointers\" (depending on the number of items
    eg. 32bit or even smaller) when referencing from other column-structures
  Restrictions:
  - Garbage collection is not available -- only reset of all data,
    or you need to keep a free list or free pointer chain
  - TODO: method dispatching doesn't work yet
  "
  (destructuring-bind (struct-name &rest options) (sb-int:ensure-list name-and-options)
    (assert (symbolp struct-name))
    (flet ((option (name default)
             (or (second (find name options :key #'first)) default)))
      (let* ((*current-cs-sym* struct-name)
             (initial-size (option :initial-size 16384))
             (batched% (option :batch-size nil))
             (batch-size (if (eq batched% t)
                             (default-batch-size initial-size)
                             batched%))
             ;;
             (index-name (sb-int:gensymify* :%index))
             (index-bits (option :index-bits nil))
             (max-bits (or (option :max-bits nil)
                           index-bits
                           32))
             (*current-cs-size* max-bits)
             ;;
             (base-constructor (option :base-constructor (sb-int:gensymify* struct-name :-constructor)))
             ;;
             (with-batch-macro (option :with-batch-allocation-name nil))
             (constructor-name (option :constructor
                                       (intern (format nil "~a~a" :make- struct-name)
                                               (symbol-package struct-name))))
             ;;
             (as-alist-sym (sb-int:gensymify* struct-name :-as-alist))
             ;;
             ;;
             (user-slots (let ((*slot-nr* 0))
                           (mapcar #'get-slot-def
                                   slot-definitions)))
             (user-slots-use-vector-storage?
               (mapcar (lambda (s)
                         (slot-def-has-some-mixin? s 'cs-mixin-vector-storage-slot))
                       user-slots))
             (vector-storage-slot-count (count-if #'identity user-slots-use-vector-storage?))
             ;;
             (needs-index-slot (or index-bits
                                   (plusp vector-storage-slot-count)
                                   (null user-slots)))
             (all-slots (if needs-index-slot
                            (list* (get-slot-def `(,index-name 0
                                                               :type (unsigned-byte ,(or index-bits 32))
                                                               :allocation :immediate))
                                   user-slots)
                            user-slots)))
        ;; A sole index slot to identify something is good enough
        (when (zerop (length all-slots))
          (error "Need at least one slot in ~s" struct-name))
        ;;
        ;; Stage 1:
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (def-udef-inttype ,struct-name
                          :max-bits ,max-bits
                          ;; NIL gets stored in U-B columns as -1 (mod bits) values
                          ; TODO configurable?
                          :nil-as-minus-1 t)
             ;;
             ;; TODO: loses value upon reload, keep old contents?
             (sb-vm:without-arena
               (setf (get ',struct-name 'column-struct-data)
                     (let ((*bit-index* sb-impl::+udef-reserved-low-bits+)
                           (*slot-nr* 0)
                           (this-size ,max-bits))
                       (declare (ignorable this-size))
                       (make-udef-c-s-metadata
                         :udef          ',struct-name
                         :batch-size    ,batch-size
                         :slots         (vector ,@ all-slots)
                         :has-index-slot ,(and needs-index-slot t)
                         :vec-slot-count ,vector-storage-slot-count
                         :data-vec      (vector ,@(loop repeat vector-storage-slot-count
                                                        collect #()))
                         :as-alist      ',as-alist-sym))))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               ;;
               ;; This macro has actual structures to query
               (expand-c-s-definition ,struct-name
                                      ,constructor-name
                                      ,base-constructor
                                      ,with-batch-macro)))
           ;; Runtime only
           (column-struct-reset ',struct-name)
           (column-struct-resize ',struct-name ,initial-size)
           ;; done
           ',struct-name)))))

(defmacro expand-c-s-definition (*current-cs-sym* constructor-name base-constructor with-batch-macro)
  (multiple-value-bind (cs udef) (get-cs-metadata-from-symbol *current-cs-sym* t)
    (with-slots (as-alist batch-size slots) cs
      (check-var-len-slots slots)
      (loop for slot across slots
            for slot-name = (cs-s-slot-name slot)
            collect slot-name into slot-names
            collect `(,slot-name ,(cs-s-init-form slot)) into maker-arg-list
            collect `(check-type ,slot-name
                                 ,(cs-s-input-type slot)) into maker-checks
            appending (getter-and-setter-functions slot cs) into accessors
            finally
            (return
              (sb-int:with-unique-names (up-to old-size len tmp numeric-index where)
                `(progn
                   ;; Accessors
                   ,@ (mapcar (lambda (form)
                                (cons 'defun form))
                              accessors)
                   ;;
                   #+(or)(declaim (inline ,base-constructor))
                   ;; First slot is the immediate index, to be returned (with other immediates) in a udef!
                   (defun ,base-constructor ,slot-names
                     (declare (optimize (debug 3)
                                        (speed 1)
                                        (compilation-speed 0)
                                        (safety 1)))
                     ;;D; ,(when (plusp (cs-meta-vec-slot-count cs))
                     ;;D;    `(declare (ignorable ,(first slot-names))))
                     ;; TODO: provide restart for reallocation
                     (let* ((,where (load-time-col-struct ',*current-cs-sym*))
                            ;; Make numeric index available via correct name
                            (,len ,(or (cs-s-slot-name
                                         (cs-meta-var-len-slot cs))
                                       1))
                            ;; For a variable-length allocation, we need that much free space
                            (,up-to (+ ,(first slot-names) ,len)))
                       (declare (notinline column-struct-size)
                                (type udef-c-s-metadata ,where))
                       (loop for ,old-size = (column-struct-size ,where)
                             while (>= ,up-to ,old-size)
                             for i from 0 below 5
                             do (column-struct-resize
                                  ,where
                                  ;; 1 level: exponential growth;
                                  ;; 2-level: only one (batch-size) element added
                                  ,(if batch-size
                                       `(+ ,up-to
                                           (cs-meta-batch-size ,where))
                                       `(max (round (* (sqrt 2)
                                                       ,up-to))
                                             (+ ,up-to
                                                50)))))
                       (when (>= ,up-to (column-struct-size ,where))
                         (error "Cannot resize ~s" ',*current-cs-sym*))
                       (let ((,tmp ,(make-udef-with-immediate-slots-form
                                      (udef-metadata-to-udef udef)
                                      slots)))
                         ;; Generate SETFs for column-struct slots
                         ,@(loop for slot across slots
                                 for name = (cs-s-slot-name slot)
                                 for gs = (sb-int:gensymify* name)
                                 unless (typep slot 'cs-mixin-immediate)
                                 collect `(setf (,(cs-s-accessor-sym slot) ,tmp)
                                                ,name) into setfs
                                 finally (return setfs))
                         ;; Return specifically tagged UDEF-INTTYPE
                         ,tmp)))
                   ;;
                   ;; User-visible constructor function.
                   (defun ,constructor-name (&key ,@ (rest maker-arg-list))
                     ,@ (rest maker-checks)
                     ;; Value before incrementing
                     ;; Asked from the _current_ metadata in case of COLUMN-STRUCT-RESET
                     (let* ((,tmp (get-cs-metadata-from-symbol ',*current-cs-sym*))
                            (,numeric-index (get-new-id-range ,tmp
                                                              ,(cs-s-slot-name
                                                                 (cs-meta-var-len-slot cs)))))
                       ;; First arg is the numeric index!
                       (,base-constructor ,numeric-index
                                          ,@ (rest slot-names))))
                   ;;
                   ;;
                   (defun ,as-alist (,tmp)
                     ;; Don't report the index
                     (with-c-s-slots (,*current-cs-sym* ,tmp) ,(rest slot-names)
                       (list
                         ,@(loop for name in (rest slot-names)
                                 collect `(cons ,(intern (symbol-name name)
                                                         :keyword)
                                                ,name)))))
                   ;;
                   ,(when with-batch-macro
                      ;; Sanity check
                      (when (not batch-size)
                        (error "WITH-BATCHED-ALLOCATION is only available in batch mode (yet)."))
                      (assert (> batch-size 3)) ;; TODO increase
                      (return-batch-macro *current-cs-sym* with-batch-macro
                                          base-constructor
                                          maker-arg-list (rest slot-names)))
                   ;;
                   ;; BROKEN: There is no class named X.
                   #+(or)
                   (defmethod sb-c::describe-object :after ((obj ,*current-cs-sym*) stream)
                     (with-c-s-slots (,*current-cs-sym* ,obj) slot-names
                       ,@(loop for name in slot-names
                               collect `(format stream "~&  ~A = ~A~%" ',name ,name)))))))))))


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

;; TODO: only immediate slots → no index column, or width 0?

;; TODO: method dispatch on inner-tagged UDEFs - it's possible on FIXNUM etc. as well!

;; TODO: box/unbox into (unsigned-byte X) specialized arrays and slots
;;       (Without needing a new array widetag for each udef-tag?)

;; TODO: (SETF (AREF )) doesn't work for vectors of udef (translation to a differently typed array!)
;; Detect and complain or even fix?
;; Would need to see _outer_ AREF?!

;; TODO: optionally a freelist
;; TODO: integration in rucksack etc.

;; TODO: #\Nul or 0 terminated data

