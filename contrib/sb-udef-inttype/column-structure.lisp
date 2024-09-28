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
;;
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
;;
;;
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
;;
;;
;; The use-case this is being implemented for is a database -
;; data gets imported into the heap, so that SAVE-LISP-AND-DIE
;; gives an instant-on service that (when saved uncompressed)
;; pages data in on demand -- even if only 2GB RAM are available
;; for a 20GB heap dump.
;;
;;
;; The savings in GC time are quite nice:
;;   # make comparison dumpfile=/tmp/my-binary
;;   DEFSTRUCT:
;;     Elapsed:   0:08.88   User: 8.66   System: 0.21
;;     Size:    298501728   Created:    8000000
;;   DEF-COLUMN-STRUCT:
;;     Elapsed:   0:02.55   User: 2.26   System: 0.29
;;     Size:    245401368   Used items: 8000001 of 8448152
;;   DEF-COLUMN-STRUCT with large initial-size:
;;     Elapsed:   0:02.48   User: 2.23   System: 0.25
;;     Size:    237534648   Used items: 8000001 of 8120000
;;   DEF-COLUMN-STRUCT with large initial-size and batched:
;;     Elapsed:   0:03.16   User: 2.99   System: 0.17
;;     Size:    235207408   Used items: 8000001 of 8000008
;;
;;
;; Keywords: headerless-structures, column-oriented-data, gc-friendly, runtime-type-safety

(defpackage :sb-column-struct
  (:use :common-lisp ;:sb-udef-inttype
        )
  (:export #:def-column-struct
           #:column-struct-size
           #:column-struct-last-index
           #:column-struct-reset
           #:column-struct-resize))
(defpackage :sb-column-struct-internal-accessors)
(in-package :sb-column-struct)

;; Sizing heuristics for parallel allocation in threads
(defun default-batch-size (guess)
  (expt 2 (ceiling (log guess 2) 4/3)))


;; for ATOMIC-INCF to work, these must be structures.
;; The slot names (rather their initargs) might collide with ones our users choose, sadly...
(defstruct (udef-c-s-metadata
             (:conc-name c-s-))
  (udef         nil :type symbol     :read-only t)
  (master-var   nil :type symbol     :read-only t)
  (e-slot-names nil :type list       :read-only t)
  (i-slot-names nil :type list       :read-only t)
  (mfill-pointer  0 :type sb-vm:word)
  (batch-size   nil :type (or null
                              (integer 1 1000000000))
                :read-only t))

(defstruct (udef-c-s-lower
             (:conc-name csl-))
  (master-var    nil :type symbol     :read-only t)
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
  (make-lower nil :type function)
  ;; lower vector slot added dynamically because of dynamic type
  )

(defstruct (udef-c-s-only
             (:include udef-c-s-metadata)
             (:conc-name cso-))
  ;; Other slots added dynamically
  )

(defun get-upper-last-batch (obj)
  (let* ((lower (funcall (csu-lower-acc obj) obj))
         (last-batch-nr (1- (length lower))))
    (aref lower last-batch-nr)))

(declaim (inline get-new-id-range))
(defun get-new-id-range (obj)
  (cond
    ((typep obj 'udef-c-s-only)
     (sb-ext:atomic-incf (c-s-mfill-pointer obj)))
    ((typep obj 'udef-c-s-upper)
     (sb-ext:atomic-incf (c-s-mfill-pointer obj)))
    ((typep obj 'udef-c-s-lower)
     (sb-ext:atomic-incf (csl-lfill-pointer obj)))
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
     (map 'nil #'column-struct-reset
          (funcall (csu-lower-acc obj) obj)))
    (t
     (error "Bad type for ~s" obj))))

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


#+(or)
(defun column-struct-get-plist (obj)
  "Returns the data of OBJ as a plist."
  (multiple-value-bind (type idx) (sb-impl::udef-inttype-type-of obj)
    (assert type)
    (let ((data (or (get type 'column-struct-data)
                    (error "~s is not a column-structure index." obj))))
      ;; todo: doesn't work because not seen as an instance, like with PRINT-OBJECT
      )))

(defun column-struct-resize (obj new-size)
  "Resizes to (at least) NEW-SIZE.
  Might do concurrency-unsafe ADJUST-ARRAY calls for a single-level column-struct,
  or just add new batches for a two-level column-struct."
  (cond
    ((symbolp obj)
     (let ((data (or (get obj 'column-struct-data)
                     (error "~s is not a column-structure type." obj))))
       (column-struct-resize (symbol-value data) new-size)))
    ;;
    ((typep obj 'udef-c-s-only)
     ;; unsafe resize, other threads might change elements
     ;; TODO: add restart to reset or keep them if more indizes are actually used
     (dolist (slot (cso-i-slot-names obj))
       (let ((old (slot-value obj slot)))
         (setf (slot-value obj slot)
               (adjust-array old new-size
                             ;;:initial-element ??
                             :element-type (array-element-type old))))))
    ;;
    ((typep obj 'udef-c-s-upper)
     (let* ((batches-wanted (ceiling new-size (c-s-batch-size obj)))
            (old (funcall (csu-lower-acc obj) obj)))
       ;; TODO: also make smaller?
       ;; Don't use ARRAY-DIMENSION, the array is ADJUSTABLE
       (sb-thread:with-mutex ((csu-lock obj))
         (loop for size-now = (length old)
               while (< size-now batches-wanted)
               for new-lower-index = (vector-push-extend
                                       (funcall (csu-make-lower obj)
                                                (csu-master-var obj)
                                                size-now)
                                       old)

               ))))
    ;;
    (t
     (error "Bad type for ~s" obj)))
  #+(or)
  (format *trace-output* "resized to ~d~&" (column-struct-size obj))
  (column-struct-size obj))


(defun c-s-value% (2-level? data-var accessor idx lower-acc &optional (before 'identity) after)
  (if 2-level?
      (sb-int:with-unique-names (batch i lower)
        `(multiple-value-bind (,batch ,i) (floor ,idx (c-s-batch-size ,data-var))
           (let ((,lower (,lower-acc ,data-var)))
             (unless (< ,batch (array-dimension ,lower 0))
               (error "Data ~s out of bounds" ,idx))
             (,before (aref (,accessor (aref ,lower ,batch))
                            ,i)
                      ,@ after))))
      `(,before (aref (,accessor ,data-var)
                      ,idx)
                ,@ after)))


(defun handle-c-s-slots (struct-name 2-level?
                                     batch-size data-var initial-size
                                     udef-reader lower-acc
                                    slots)
  ;; This ensures that we get #:SLOT-1, #:SLOT-2, for each column-structure,
  ;; so doesn't pollute the keyword package for the initargs as much
  (let ((*gensym-counter* 1))
    ;; both &optional and &key
    (declare (sb-ext:muffle-conditions style-warning))
    (loop for slot% in slots
          for (ext-name init type) = (destructuring-bind
                                         (n &optional i &key (type t))
                                         (sb-int:ensure-list slot%)
                                       (list n i type))
          ;; TODO: :CONC-NAME behaviour
          ;; This is actually incompatible -- we should intern into *PACKAGE*??!
          for e-accessor = (intern (format nil "~a-~a" struct-name ext-name)
                                   (symbol-package struct-name))
          ;; To avoid collisions, we generate own internal slot names
          for i-name = (gensym (symbol-name ext-name))
          collect i-name into i-slot-names
          collect ext-name into e-slot-names
          collect `(,i-name
                     (make-array (list ,(or batch-size initial-size))
                                 :initial-element ,init
                                 :element-type ',type)
                     :type (simple-array ,type (,(or batch-size '*))))
          into actual-slots
          collect `(,ext-name ,init) into constructor-arg-list
          collect e-accessor into e-accessor-names
          ;;
          ;; TODO: inlines?
          collect `(declare (ftype (function (,struct-name) ,type) ,e-accessor))
          into decl
          collect `(defun ,e-accessor (id)
                     (declare (optimize (speed 3) (safety 1) (debug 1))
                              (type ,struct-name id))
                     (let ((idx (,udef-reader id)))
                       ,(c-s-value% 2-level? data-var i-name 'idx lower-acc)))
          into code
          collect `(declare (ftype (function (,type ,struct-name) ,type) ,e-accessor))
          into decl
          collect `(defun (setf ,e-accessor) (new-val id)
                     (declare (optimize (speed 3) (safety 1) (debug 1))
                              (type ,struct-name id))
                     (let ((idx (,udef-reader id)))
                       ,(c-s-value% 2-level? data-var i-name 'idx lower-acc
                                    'setf '(new-val)))
                     new-val)
          into code
          finally (return (values i-slot-names e-slot-names
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
      (sb-int:with-unique-names (old-size udef-maker udef-reader idx where)
        (let* ((initial-size (option :initial-size 16384))
               (udef-inttype-id (option :udef-inttype-id nil))
               (batched% (option :batched nil))
               (batch-size (if (eq batched% t)
                               (default-batch-size initial-size)
                               batched%))
               ;;
               (2-level? (if batch-size t nil))
               ;;
               (with-batch-macro (option :with-batch-allocation-sym nil))
               (ll-constructor (gensym (format nil "~a-~a" :make-ll struct-name)))
               (var-constructor (gensym (format nil "~a-~a" :make-data-var struct-name)))
               (constructor-name (option :constructor
                                         (intern (format nil "~a~a" :make- struct-name)
                                                 (symbol-package struct-name))))
               ;;
               (col-struct (gensym (format nil "~a-~a" :col-struct struct-name)))
               (upper-struct (gensym (format nil "~a-~a" :upper-col-s struct-name)))
               (data-var (option :data-var (gensym (format nil "~a-~a" :data struct-name))))
               (base-constructor (gensym (format nil "*~a-CONSTRUCTOR*" struct-name)))
               ;;
               (lower-acc (gensym "LOWER")))
          (when (zerop (length slots))
            (error "Need at least one slot in ~s" struct-name))
          (multiple-value-bind (i-slot-names e-slot-names
                                             e-accessor-names actual-slots
                                             constructor-arg-list code decl)
              (handle-c-s-slots struct-name 2-level?
                                batch-size data-var initial-size
                                udef-reader lower-acc
                                slots)
            (let ((base-defaults `((master-var ',data-var)
                                   (batch-size ,batch-size)
                                   (i-slot-names ',i-slot-names)
                                   (e-slot-names ',e-slot-names)
                                   (udef ',struct-name))))
              (unless 2-level?
                (setf upper-struct col-struct))
              (identity ;sb-ext:with-current-source-form (options slots)
                `(progn
                   ,(if 2-level?
                        `(progn
                           (defstruct (,col-struct
                                        (:constructor ,ll-constructor
                                         (master-var batch-index ; &optional ,@ i-slot-names
                                                     ))
                                        (:conc-name nil)
                                        (:include udef-c-s-lower
                                         (master-var ',data-var)))
                             ,@ actual-slots)
                           (defstruct (,upper-struct
                                        (:constructor ,var-constructor)
                                        (:conc-name nil)
                                        (:include udef-c-s-upper
                                         (lower-acc #',lower-acc)
                                         (make-lower #',ll-constructor)
                                         ,@ base-defaults))
                             (,lower-acc
                               (make-array 1
                                           :element-type ',col-struct
                                           :adjustable t
                                           :fill-pointer 1
                                           :initial-contents
                                           (list (,ll-constructor ',data-var 0))))))
                        ;; single-level only
                        `(defstruct (,col-struct
                                      (:constructor ,var-constructor
                                       (&optional ,@ i-slot-names))
                                      (:conc-name nil)
                                      (:include udef-c-s-only
                                       ,@ base-defaults))
                           ,@ actual-slots))
                   ;;
                   ;;
                   (deftype ,struct-name () 'sb-int:udef-inttype)
                   (sb-impl::def-udef-inttype ,struct-name
                     :id ,udef-inttype-id
                     ;; TODO: derive max-bits
                     :constructor ,udef-maker
                     :reader ,udef-reader)
                   ;;
                   (setf (get ',struct-name 'column-struct-data)
                         ',data-var)
                   ;;
                   (declaim (type ,(if 2-level?
                                       upper-struct
                                       col-struct)
                                  ,data-var))
                   (sb-ext:defglobal ,data-var (,var-constructor))                   ;;
                   ;;
                   (declaim (inline ,base-constructor))
                   (defun ,base-constructor (,where ,idx ,@ e-slot-names)
                     ;; TODO: provide restart for reallocation
                     (let ((,old-size (column-struct-size ,where)))
                       (when (>= ,idx ,old-size)
                         (column-struct-resize ,where
                                               (max (round (* (sqrt 2) ,old-size))
                                                    (+ ,old-size
                                                       ,(or batch-size 50))))))
                     ,@ (mapcar
                            (lambda (int ext)
                              (c-s-value% 2-level? data-var int idx lower-acc 'setf (list ext)))
                            i-slot-names
                            e-slot-names)
                     ;; Return (doubly-)tagged UDEF-INTTYPE
                     (,udef-maker ,idx))
                   ;; This arglist must be in correct order, though
                   (defun ,constructor-name (&key ,@ constructor-arg-list)
                     ;; Value before incrementing
                     (let ((,idx (get-new-id-range ,data-var)))
                       (,base-constructor ,data-var ,idx ,@ e-slot-names)))
                   ;;
                   ,(when with-batch-macro
                      (assert (> 100 batch-size))
                      #+(or)
                      `(defmacro ,with-batch-macro ((fn-name) &body body)
                         ,(format nil "FN-NAME is a local constructor taking the same arguments as ~a,
                                  with a locally reserved range of IDs so that threads operate
                                  on different cache lines." constructor-name)
                         ;; TODO: another vector, one level above to avoid reallocating big vectors
                         ;; and that function allocates a new lower-level vector
                         (let ((,local-alloc-max -1)
                               (,local-alloc-idx 0))
                           (flet ((alloc ()
                                    (multiple-value-bind (new max) (get-new-id-range ,data-var ,batch-size)
                                      (setf ,local-alloc-max max
                                            ,local-alloc-idx new)))
                                  (fn-name ()
                                    (when (>= ,local-alloc-idx ,local-alloc-max)
                                      (alloc))
                                    (prog1
                                        ,local-alloc-idx
                                      (incf ,local-alloc-idx))))
                             (locally
                               ,@ `, body)))))

                      ;;
                      (locally
                        ,@ decl)
                      ,@ code
                      ;;
                      ;; BROKEN: There is no class named X.
                      ',e-accessor-names ; avoid unused warning
                      #+(or)
                      (defmethod sb-c::describe-object :after ((obj ,struct-name) stream)
                        ,@(loop for acc in e-accessor-names
                                for slot in i-slot-names
                                collect `(format stream "~&  ~A = ~A~%" ',slot (,acc obj))))
                      ;;
                      (column-struct-reset ,data-var)
                      (column-struct-resize ,data-var ,initial-size)
                      ',struct-name)))))))))

;; TODO: don't EVAL, but return one form using EVAL-WHEN?
;;       should get rid of (OR NULL ...) in DECLAIM
;;       Also the WARNING:
;;          The new TYPE proclamation for COL-STRUCT-EXAMPLE::MY-FOO-DATA
;;               (OR NULL #:COL-STRUCT-FOO337)
;;          does not match the old TYPE proclamation
;;               (OR NULL #:COL-STRUCT-FOO231)
;;
;; TODO: box/unbox into (unsigned-byte X) specialized arrays and slots
;; TODO: optionally a freelist
;; TODO: optimizing, especially the tags -- check all 16bit at once
