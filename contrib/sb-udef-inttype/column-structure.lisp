;;; Define a column-structure.
;;
;; The slot definitions passed in are used in a DEFCLASS
;; whose slots contain vectors;
;; the structure name is used to create a UDEF-INTTYPE,
;; so that other structures can reference instances here.
;;
;; You can either create one set of (large) vectors,
;; which will mean some load when resizing
;; (and concurrent modification while resizing will lose changes!);
;; or a 2-level structure, where one vector just stores class instances
;; with (fixed-size) vectors for the data.
;; In the latter case allocating more data means just adding to the first vector,
;; so that should be multithreading-safe.


;; Simple case:
;;     ┌global-var─────┐
;;     │ udef          │
;;     │ mfill-pointer │
;;     │ master-var    │
;;     │ ...           │
;;     │ #:slot0       ├─────────────>┌vect0┐
;;     │ #:slot1       ├────>┌vect1┐  │v-0-0│
;;     └───────────────┘     │v-0-1│  │v-1-0│
;;                           │v-1-1│  │v-2-0│
;;                           │v-2-1│  │     │
;;                           │     │  └─────┘
;;                           └─────┘
;;
;; 2-level-case:
;;
;;     ┌───────────────┐<──────┐
;;     │ master-var    │       │     ┌───────────────┐  ┌vect0┐
;;     │ udef          │       └─────┤ master-var    │  │v-0-0│
;;     │ mfill-pointer │             │ batch-index   │  │v-1-0│
;;     │ ...           │             │ lfill-pointer │  │v-2-0│
;;     │ #:lower       ├─>┌batches┐  │ #:slot-0      ├─>└─────┘
;;     └───────────────┘  │ 0     ├─>│ #:slot-1      ├─>┌vect1┐
;;                        │ 1     ├┐ │               │  │v-0-1│
;;                        │       ││ └───────────────┘  │v-1-1│
;;                        │       │└──>└─────────────┘  │v-2-1│
;;                        └───────┘                     └─────┘
;;
;;
;; The structures being used are defined freshly,
;; so that the slot and vector types are as narrow as possible.
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

;; for ATOMIC-INCF to work, these must be structures.
;; The slot names (rather their initargs) might collide with ones our users choose, sadly...
(defstruct (udef-c-s-metadata
             (:conc-name c-s-)
             (:include udef-metadata))
  (udef         nil :type symbol     :read-only t)
  (master-var   nil :type symbol     :read-only t)
  (e-slot-names nil :type list       :read-only t)
  (i-slot-names nil :type list       :read-only t)
  (e-acc-fn     nil :type list       :read-only t)
  (elem-counts  nil :type list       :read-only t)
  (slot-acc-fn  nil :type list       :read-only t)
  (mfill-pointer  0 :type sb-vm:word)
  (batch-size   nil :type (or null
                              (integer 1 1000000000))
                :read-only t))

(defstruct (udef-c-s-lower
             (:conc-name csl-))
  ;; TODO: circular definition
  (upper         nil :type T #+(or)udef-c-s-upper     :read-only t)
  (batch-index     0 :type fixnum)
  (lfill-pointer   0 :type sb-vm:word))

(defstruct (udef-c-s-upper
             (:include udef-c-s-metadata)
             (:conc-name csu-))
  (lock (sb-thread:make-mutex :name "c-s-upper-lock")
        :type sb-thread:mutex
        :read-only t)
  ;; As the derived specific structures get created with :CONC-NAME NIL,
  ;; we cannot use a common symbol like LOWER for the slot name --
  ;; the accessors would conflict with each other.
  ;; And :INCLUDE doesn't allow overriding the type
  ;; (which differs)...
  (lower-acc nil :type function)
  (set-lower-acc nil :type function)
  (make-lower nil :type function)
  ;; lower vector slot added dynamically because of dynamic type --
  ;; must be a GENSYM so that there's no conflict for multiple
  ;; DEF-COLUMN-STRUCTs.
  )

(defstruct (udef-c-s-only
             (:include udef-c-s-metadata)
             (:conc-name cso-))
  ;; Other slots added dynamically
  )

(defun get-udef-metadata-from-symbol (sym)
  (let ((v (or (get sym 'column-struct-data)
               (get sym 'udef-metadata))))
    (when v
      (etypecase v
        (symbol (symbol-value v))
        (udef-metadata v)))))

(defun get-upper-last-batch (obj)
  (let* ((lower (funcall (csu-lower-acc obj) obj))
         (last-batch-nr (1- (length lower))))
    (aref lower last-batch-nr)))

;(declaim (inline get-new-id-range))
(defun get-new-id-range (obj &optional (count 1))
  (cond
    ((typep obj 'udef-c-s-only)
     (sb-ext:atomic-incf (c-s-mfill-pointer obj)
                         count))
    ((typep obj 'udef-c-s-upper)
     (sb-ext:atomic-incf (c-s-mfill-pointer obj)
                         count))
    ((typep obj 'udef-c-s-lower)
     (sb-ext:atomic-incf (csl-lfill-pointer obj)
                         count))
    (t
     (error "Bad type for ~s" obj))))

(defun column-struct-reset (obj)
  "Soft-resets OBJ, ie. sets the last used index to 0."
  (cond
    ((symbolp obj)
     (let ((data (or (get obj 'column-struct-data)
                     (error "~s is not a column-structure type." obj))))
       (column-struct-reset (symbol-value data))))
    ((typep obj 'udef-c-s-only)
     (setf (c-s-mfill-pointer obj) 0))
    ((typep obj 'udef-c-s-lower)
     (setf (csl-lfill-pointer obj) 0))
    ((typep obj 'udef-c-s-upper)
     (setf (c-s-mfill-pointer obj) 0)
     (map 'nil #'column-struct-reset
          (funcall (csu-lower-acc obj) obj)))
    (t
     (error "Bad type for ~s" obj))))

(declaim (ftype (function (t) (sb-int:index)) column-struct-size))
(defun column-struct-size (obj)
  "Returns the allocated length (not the number of elements used)"
  (cond
    ((symbolp obj)
     (let ((data (or (get obj 'column-struct-data)
                     (error "~s is not a column-structure type." obj))))
       (column-struct-size (symbol-value data))))
    ((typep obj 'udef-c-s-only)
     (array-dimension (slot-value obj
                                  (first
                                    (cso-i-slot-names obj)))
                      0))
    ((typep obj 'udef-c-s-upper)
     (* (length (funcall (csu-lower-acc obj) obj))
        (csu-batch-size obj)))
    (t
     (error "Bad type for ~s" obj))))


(defun column-struct-last-index (obj)
  "Returns the last used index;
  doesn't care about fragmentation with batched allocation."
  (cond
    ((symbolp obj)
     (let ((data (or (get obj 'column-struct-data)
                     (error "~s is not a column-structure type." obj))))
       (column-struct-last-index (symbol-value data))))
    ((typep obj 'udef-c-s-only)
     (c-s-mfill-pointer obj))
    ((typep obj 'udef-c-s-upper)
     (c-s-mfill-pointer obj))
    #+(or)
    ((typep obj 'udef-c-s-lower)
     (let* ((last-batch (get-upper-last-batch obj)))
       (+ (* (csl-batch-index last-batch)
             (csu-batch-size obj))
          (csl-mfill-pointer last-batch))))
    (t
     (error "Bad type for ~s" obj))))


(defun column-struct-get-struct (obj &key (output :alist))
  "Returns the data of OBJ as an alist."
  (multiple-value-bind (type idx) (udef-inttype-type-of obj)
    ;; Optimizations possible
    (declare (ignore idx))
    (assert type)
    (let* ((master-var (or (get type 'column-struct-data)
                           (error "~s is not a column-structure index." obj)))
           (udef (symbol-value master-var)))
      (loop for name in (c-s-e-slot-names udef)
            for acc-fn in (c-s-e-acc-fn udef)
            if (eq output :alist)
            ;; TODO: keywords or the actual slot symbols?
            collect (cons name (funcall acc-fn obj))))))

(defun column-struct-resize (obj new-size &key force-smaller)
  "Resizes to (at least) NEW-SIZE.
  Might do concurrency-unsafe ADJUST-ARRAY calls for a single-level column-struct,
  or just add new batches for a two-level column-struct."
  (cond
    ((symbolp obj)
     (let ((data (or (get obj 'column-struct-data)
                     (error "~s is not a column-structure type." obj))))
       (column-struct-resize (symbol-value data) new-size
                             :force-smaller force-smaller)))
    ;;
    ((typep obj 'udef-c-s-only)
     ;; unsafe resize, other threads might change elements
     ;; TODO: add restart to reset or keep them if more indizes are actually used?
     (when (or force-smaller
               (> new-size (column-struct-size obj)))
       (dolist (slot (cso-i-slot-names obj))
         (let ((old (slot-value obj slot)))
           (setf (slot-value obj slot)
                 (adjust-array old new-size
                               ;;:initial-element ??
                               :element-type (array-element-type old)))))))
    ;;
    ((typep obj 'udef-c-s-upper)
     (sb-thread:with-mutex ((csu-lock obj))
       ;; Get data only after acquiring the lock!
       (let* ((batches-wanted (ceiling new-size (c-s-batch-size obj)))
              (old (funcall (csu-lower-acc obj) obj)))
         (cond
           ;; Decrease size
           ((and force-smaller
                 (> (array-dimension old 0)
                    batches-wanted))
                       #+(or)
            (format *trace-output* "~d: shrinking from ~d to ~d batches~%"
                    (sb-thread:thread-os-tid sb-thread:*current-thread*)
                    (array-dimension old 0)
                    batches-wanted)
            (setf (fill-pointer old) batches-wanted)
            (sb-vm:%write-barrier)
            (funcall (csu-set-lower-acc obj)
                     (adjust-array old batches-wanted
                                   :fill-pointer batches-wanted
                                   :element-type (array-element-type old))
                     obj))
           ;; Increase size.
           ;; VECTOR-PUSH-EXTEND isn't safe against concurrent readers.
           ((< (array-dimension old 0)
               batches-wanted)
            (let ((new-vec (make-array batches-wanted
                                       :fill-pointer 0
                                       :element-type (array-element-type old)))
                  (old-cnt (fill-pointer old)))
              ;; Say we're about to write elements
              (setf (fill-pointer new-vec)
                    batches-wanted)
              (replace new-vec old :end1 old-cnt)
              (loop for i from old-cnt below batches-wanted
                    for c-s = (funcall (csu-make-lower obj) i)
                       #+(or)
                    do
                       #+(or)
                    (princ
                         (format nil "~d: resize from ~d to ~d: add ~d at 0x~x~%"
                               (sb-thread:thread-os-tid sb-thread:*current-thread*)
                               old-cnt batches-wanted i
                               (sb-kernel:get-lisp-obj-address c-s))
                         *trace-output*)
                    do (setf (aref new-vec i) c-s))
              ;; Ensure visibility order
              (sb-vm:%write-barrier)
              ;; Write vector
              (funcall (csu-set-lower-acc obj)
                       new-vec
                       obj)))
           ;; Just use existing index.
           ((< (fill-pointer old)
               batches-wanted)
                  (format *trace-output* "~d: upgrade ~d to ~d~%"
                             (sb-thread:thread-os-tid sb-thread:*current-thread*)
                             (fill-pointer old) batches-wanted)
            (loop for i from (fill-pointer old) below batches-wanted
                  for c-s = (funcall (csu-make-lower obj) i)
                       #+(or)
                  do
                       #+(or)
                  (format *trace-output* "~d: reserve ~d~%"
                             (sb-thread:thread-os-tid sb-thread:*current-thread*) i)
                  do (setf (aref old i) c-s))
            #+(or)
            (sb-vm:%write-barrier)
            (setf (fill-pointer old)
                  batches-wanted))))))
    ;;
    (t
     (error "Bad type for ~s" obj)))
  #+(or)
  (format *trace-output* "~d: resized to ~d~&"
          (sb-thread:thread-os-tid sb-thread:*current-thread*)
          (column-struct-size obj))
  (column-struct-size obj))

(defun column-struct-clear (obj &key new-initial-size)
  "Hard-resets OBJ, ie. drops storage vectors."
  (column-struct-reset obj)
  (column-struct-resize obj 0 :force-smaller t)
  (when new-initial-size
    (column-struct-resize obj new-initial-size))
  new-initial-size)

(defun c-s-value% (2-level? data-var accessor idx elem-count lower-acc &optional (before 'identity) after)
  (flet ((aref-or-subseq (form idx)
           (if (= 1 elem-count)
               `(,before (aref ,form ,idx)
                         ,@ after)
               (sb-int:with-unique-names (idx2)
                 `(let ((,idx2 (* ,idx ,elem-count)))
                    (,before
                       (subseq ,form ,idx2 (+ ,idx2 ,elem-count))
                       ,@ after))))))
    (if 2-level?
        (sb-int:with-unique-names (batch i lower)
          `(multiple-value-bind (,batch ,i) (floor ,idx (c-s-batch-size ,data-var))
             (let ((,lower (,lower-acc ,data-var)))
               #+(or)
               (unless (< ,batch (length ,lower 0))
                 (error "Data ~s out of bounds" ,idx))
               ,(aref-or-subseq `(,accessor (aref ,lower ,batch))
                                i))))
        (aref-or-subseq `(,accessor ,data-var)
                        idx))))


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


(defun array-type-destructure (type% init)
  "Returns vector type, slot type, argument type, element count, init form, single init value,
  and whether a conversion from/to UNSIGNED-BYTE should happen"
  (if (and (consp type%)
           (member (first type%) '(array vector simple-vector))
           (numberp (third type%)))
      (destructuring-bind (kind entry-type dimensions) type%
        (let ((dim-list (sb-int:ensure-list dimensions)))
          (list entry-type
                type%
                (list kind
                      entry-type
                      ;; TODO: dimension determining broken, currently only 1 dimension supported
                      '(*)
                      #+(or)
                      ;;   Definition's declared type for variable NEW-VAL:
                      ;;     (ARRAY (UNSIGNED-BYTE 32) (* * *))
                      (if (numberp dimensions)
                          dim-list
                          dimensions))
                (apply #'* dim-list)
                `(make-array (list ,@dim-list)
                             :initial-element ,init
                             :element-type ',entry-type)
                init
                nil nil)))
     (let* ((meta (and (symbolp type%)
                  (get-udef-metadata-from-symbol type%)))
            (type (if meta
                      `(unsigned-byte ,(udef-metadata-max-bits meta))
                      type%))
            (init2 (if meta
                       `(,(udef-metadata-from-udef meta) ,init)
                       init)))
            ;; Is a UDEF, so use (UNSIGNED-BYTE x) instead
       (list type type type%
             1
             init2
             init2
             (and meta (udef-metadata-from-udef meta))
             (and meta (udef-metadata-to-udef  meta))))))

(defun handle-c-s-slots (struct-name 2-level?
                                     batch-size data-var initial-size
                                     from-udef lower-acc
                                     slots)
  ;; This ensures that we get #:SLOT-1, #:SLOT-2, for each column-structure,
  ;; so doesn't pollute the keyword package for the initargs as much
  ;; both &optional and &key
  (let* ((normalized-slots (locally
                             (declare (sb-ext:muffle-conditions style-warning))
                             (mapcar
                               (lambda (x)
                                 (destructuring-bind (n &optional i &key (type t))
                                     (sb-int:ensure-list x)
                                   (list n i type)))
                               slots)))
         (i-slot-names (let ((*gensym-counter* 1))
                         (loop for (name . rest) in normalized-slots
                               collect (gensym (symbol-name name))
                               #+(or)(gensym "SLOT-")
                               ))))
    (loop for (ext-name init% type%) in normalized-slots
          ;; TODO: :CONC-NAME behaviour
          for e-accessor = (sb-int:symbolicate struct-name "-" ext-name)
          ;; To avoid collisions, we generate own internal slot names.
          ;; This also reduces pollution in the keyword package by having
          ;; always the same sequential names -- and inheriting COLUMN-STRUCTs
          ;; isn't possible (yet) anyhow.
          ;; At least that's the idea -- doesn't seem to work as expected:
          ;;   #<SB-KERNEL:DEFSTRUCT-DESCRIPTION #:COL-STRUCT-BAR648 {1003B67F33}>
          ;;   '((:SLOT T . 0) (:SLOT T . 1) (:SLOT WORD . 2) (:SLOT T . 3)
          ;;     (:SLOT T . 4) (:SLOT T . 5) (:SLOT T . 6) (:SLOT T . 7))
          for i-name in i-slot-names
          for (type slot-type atype elem-count init single-init ub-reader ub-maker)
              = (array-type-destructure type% init%)
          collect elem-count into elem-counts
          collect ext-name into e-slot-names
          collect `(,i-name
                     (make-array (list (* ,elem-count ,(or batch-size initial-size)))
                                 :initial-element ,single-init
                                 :element-type ',type)
                     :type (simple-array ,type (,(if batch-size
                                                     (* elem-count batch-size)
                                                     '*))))
          into actual-slots
          collect `(,ext-name ,init) into constructor-arg-list
          collect e-accessor into e-accessor-names
          ;;
          collect `(declaim (ftype (function (,struct-name) ,atype) ,e-accessor)
                            #+(or)(inline ,e-accessor))
          into decl
          collect `(defun ,e-accessor (id)
                       ;; TODO: check-type
                       (declare (optimize (speed 3) (safety 1) (debug 1))
                                (type ,struct-name id))
                       (let ((idx (,from-udef id)))
                         (,(or ub-maker 'identity)
                           ,(c-s-value% 2-level? data-var i-name 'idx elem-count lower-acc))))
          into code
          collect `(declaim (ftype (function (,atype ,struct-name) ,atype) (setf ,e-accessor))
                            #+(or)(inline (setf ,e-accessor)))
          into decl
          collect `(defun (setf ,e-accessor) (new-val id)
                     ;; TODO: check-type
                     (declare (optimize (speed 3) (safety 1) (debug 1))
                              (type ,struct-name id)
                              (type ,atype new-val))
                     (let ((idx (,from-udef id))
                           (new (,(or ub-reader 'identity)
                                  new-val)))
                       ,(c-s-value% 2-level? data-var i-name 'idx elem-count lower-acc
                                    'setf '(new)))
                     new-val)
          into code
          finally (return (values i-slot-names e-slot-names elem-counts
                                  e-accessor-names actual-slots
                                  constructor-arg-list code decl)))))

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
                 (batched% (option :batched nil))
                 (batch-size (if (eq batched% t)
                                 (default-batch-size initial-size)
                                 batched%))
                 ;;
                 (udef-id (or old-id
                                      (option :udef-inttype-id nil)))
                 (from-udef (or old-from-u
                                  (option :from-udef
                                          (sb-int:gensymify* struct-name :-FROM-UDEF))))
                 (to-udef (or old-to-u
                                 (option :to-udef
                                         (sb-int:gensymify* struct-name :-TO-UDEF))))
                 (udef-typep (or old-typep
                                 (option :udef-typep
                                         (sb-int:gensymify* struct-name :-TYPE-P))))
                 (max-bits (or old-bits
                               (option :max-bits 32)))
                 ;;
                 (base-constructor (option :base-constructor (sb-int:gensymify* struct-name "CONSTRUCTOR")))
                 ;;
                 (2-level? (if batch-size t nil))
                 ;;
                 (with-batch-macro (option :with-batch-allocation-name nil))
                 (ll-constructor (gensym (format nil "~a-~a" :make-ll struct-name)))
                 (var-constructor (gensym (format nil "~a-~a" :make-data-var struct-name)))
                 (constructor-name (option :constructor
                                           (intern (format nil "~a~a" :make- struct-name)
                                                   (symbol-package struct-name))))
                 ;;
                 (col-struct (gensym (format nil "~a-~a" :col-struct struct-name)))
                 (upper-struct (gensym (format nil "~a-~a" :upper-col-s struct-name)))
                 (data-var (option :data-var (gensym (format nil "~a-~a" :data struct-name))))
                 ;;
                 (lower-acc (gensym "LOWER")))
            (when (zerop (length slots))
              (error "Need at least one slot in ~s" struct-name))
            (multiple-value-bind (i-slot-names e-slot-names elem-counts
                                               e-accessor-names actual-slots
                                               constructor-arg-list code decl)
                (handle-c-s-slots struct-name 2-level?
                                  batch-size data-var initial-size
                                  from-udef lower-acc
                                  slots)
              (let ((base-defaults `((master-var ',data-var)
                                     (batch-size ,batch-size)
                                     (i-slot-names ',i-slot-names)
                                     (elem-counts ',elem-counts)
                                     (e-slot-names ',e-slot-names)
                                     (e-acc-fn ',e-accessor-names)
                                     (type-p ',udef-typep)
                                     (to-udef ',to-udef)
                                     (from-udef ',from-udef)
                                     (max-bits ',max-bits)
                                     (udef ',struct-name))))
                (unless 2-level?
                  (setf upper-struct col-struct))
                (identity ;sb-ext:with-current-source-form (options slots)
                  `(progn
                     ;;(deftype ,struct-name () 'sb-int:udef-inttype)
                     (def-udef-inttype ,struct-name
                       :id ,udef-id
                       :max-bits ,max-bits
                       ;; NIL gets stored in U-B columns as -1 (mod bits) values
                       :nil-as-minus-1 t ; TODO configurable?
                       :to-udef ,to-udef
                       :from-udef ,from-udef)
                     ;;
                     ,(if 2-level?
                          `(progn
                             (defstruct (,col-struct
                                          (:constructor ,ll-constructor
                                           (batch-index &aux (upper (symbol-value ',data-var))
                                                        ; &optional ,@ i-slot-names
                                                        ))
                                          (:conc-name nil)
                                          (:include udef-c-s-lower))
                               ,@ actual-slots)
                             (defstruct (,upper-struct
                                          (:constructor ,var-constructor)
                                          (:conc-name nil)
                                          (:include udef-c-s-upper
                                           (lower-acc #',lower-acc)
                                           (set-lower-acc #'(setf ,lower-acc))
                                           (make-lower #',ll-constructor)
                                           ,@ base-defaults))
                               (,lower-acc
                                 (make-array (list 0)
                                             :element-type ',col-struct
                                             ;; Must be empty at first, so that ,ll-constructor
                                             ;; doesn't stumble over the unbound data-var
                                             :fill-pointer 0
                                             ))))
                          ;; single-level only
                          `(defstruct (,col-struct
                                        (:constructor ,var-constructor
                                         (&optional ,@ i-slot-names))
                                        (:conc-name nil)
                                        (:include udef-c-s-only
                                         ,@ base-defaults))
                             ,@ actual-slots))
                     ;;
                     (setf (get ',struct-name 'column-struct-data)
                           ',data-var)
                     ;;
                     (declaim (type ,(if 2-level?
                                         upper-struct
                                         col-struct)
                                    ,data-var)
                              (sb-ext:global ,data-var))
                     ;; DEFGLOBAL doesn't overwrite a value...
                     (setf ,data-var (,var-constructor))
                     (declaim (sb-ext:always-bound ,data-var))
                     ;;
                     #+(or)(declaim (inline ,base-constructor))
                     (defun ,base-constructor (,where ,idx ,@ e-slot-names)
                       (declare (optimize (debug 1)
                                          (speed 3)
                                          (compilation-speed 0)
                                          (safety 1))
                                (type fixnum ,idx)
                                (type ,(if 2-level?
                                           upper-struct
                                           col-struct) ,where))
                       ;(sb-sys:without-gcing
                       ;(sb-sys:without-interrupts
                       ;; TODO: provide restart for reallocation
                       (locally
                         (declare (notinline column-struct-size))
                         (loop for ,old-size = (column-struct-size ,where)
                               while (>= ,idx ,old-size)
                               for i from 0 below 5
                               do (sb-thread:barrier (:memory)
                                    (sb-thread:barrier (:compiler)
                                      (column-struct-resize ,where
                                                            (max (round (* (sqrt 2) ,old-size))
                                                                 (+ ,old-size
                                                                    ,(or batch-size 50))))))
                               #+(or)
                               (when ,2-level?
                                 (princ
                                   (format nil "~d: ~d > ~d, resized step ~d: ~d, ~d~%"
                                           (sb-thread:thread-os-tid sb-thread:*current-thread*)
                                           ,idx ,old-size
                                           i
                                           (column-struct-size ,where)
                                           (length (funcall (csu-lower-acc ,where) ,where))
                                           )
                                   *trace-output*))
                               ))
                       #+(or)
                       (princ
                         (format nil "~d: ~d in batch ~d, 0x~x"
                                 (sb-thread:thread-os-tid sb-thread:*current-thread*)
                                 ,idx fgg
                                 (aref (funcall (csu-lower-acc ,where) ,where) fgg))
                         *trace-output*)
                       ;; TODO: for a 2-level structure the macro expansion
                       ;; repeats the (FLOOR idx batch-size) for each slot -
                       ;; is that optimized away or needs to be fixed?
                       ,@ (mapcar
                            (lambda (int ext e-c)
                              (c-s-value% 2-level? where ; data-var  ;; ,where ???
                                          int idx e-c lower-acc
                                          'setf (list ext)))
                            i-slot-names
                            e-slot-names
                            elem-counts)
                       ;; Return (doubly-)tagged UDEF-INTTYPE
                       (,to-udef ,idx))
                          ;))
                          ;; This arglist must be in correct order, though
                          (defun ,constructor-name (&key ,@ constructor-arg-list)
                            ;; Value before incrementing
                            (let ((,idx (get-new-id-range ,data-var)))
                              (,base-constructor ,data-var ,idx ,@ e-slot-names)))
                          ;;
                          ,@ decl
                          ,@ code
                          ;;
                          ,(when with-batch-macro
                             ;; Sanity check
                             (when (not batch-size)
                               (error "WITH-BATCHED-ALLOCATION is only available in batch mode (yet)."))
                             (assert (> batch-size 3)) ;; TODO increase
                             (return-batch-macro col-struct with-batch-macro
                                                 data-var batch-size base-constructor
                                                 constructor-arg-list e-slot-names))
                          ',e-accessor-names ; avoid unused warning
                          ;; BROKEN: There is no class named X.
                          #+(or)
                          (defmethod sb-c::describe-object :after ((obj ,struct-name) stream)
                            ,@(loop for acc in e-accessor-names
                                    for slot in i-slot-names
                                    collect `(format stream "~&  ~A = ~A~%" ',slot (,acc obj))))
                          ;;
                          (column-struct-reset ,data-var)
                          (column-struct-resize ,data-var ,initial-size)
                          ',struct-name))))))))))


(defmacro make-wrapped-udef-accessor (new old col-struct-name)
  "Wraps the (UNSIGNED x) value returned by (OLD obj) in (NEW obj)
  so that it becomes COLUMN-STRUCT-TYPE.
  If there's a (SETF OLD), a corresponding wrapper is created as well."
  (let* ((fn-type (sb-introspect:function-type old))
         (ret% (third fn-type))
         (ret-type (if (and (consp ret%)
                            (eq 'values (first ret%)))
                       (second ret%)
                       ret%))
         (udef (get-udef-metadata-from-symbol col-struct-name))
         (old-setf (fboundp `(setf ,old))))
    (assert udef)
    (assert (eq (first ret-type) 'unsigned-byte))
    ;; Must have space for at least as many bits
    (assert (>= (second ret-type) (c-s-max-bits udef)))
    `(progn
       (declaim (inline ,new)
                ;(ftype (function ,(second fn-type)) (values ,col-struct-name)) ,new)
                )
       ;; TODO: &rest or other args possible? Not on a getter, right?
       (defun ,new (obj)
         (,(c-s-to-udef udef)
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
                       (,(c-s-from-udef udef) ,(first req)))
                 ,(first req)))))
       ',new)))



;; TODO: EVAL-WHEN for forms in macro?

;; TODO: method dispatch on UDEFs - it's possible on FIXNUM etc. as well!
;; then fix PRINT-UGLY?

;; TODO: option to allocate a NULL element,
;; so that a slot can be a pure UNSIGNED-BYTE,
;; but getter/setter convert NIL to/from this special value (-1?)

;; TODO: box/unbox into (unsigned-byte X) specialized arrays and slots
;;       (Would that need a new widetag for each udef-tag?)

;; TODO: optimizing, especially the tags -- check all 16bit at once


;; TODO: optionally a freelist
;; TODO: integration in rucksack etc.
