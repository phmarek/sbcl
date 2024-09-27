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
;;
;; The biggest advantage is that with typed (UNSIGNED-BYTE x) slots
;; the GC load and memory usage is reduced, as vectors with such types
;; are very quickly handled.
;;
;; The advantage over just allocating big (UNSIGNED-BYTE 8) buffers and defining
;; accessor functions over integer indizes is that this solution is runtime-type-safe,
;; as at any point it is clear which data type hides behind some column-struct-index.
;;
;; Keywords: no-header-structures, column-oriented-data, gc-friendly, runtime-type-safety
;;

(defpackage :sb-column-struct
  (:use :common-lisp ;:sb-udef-inttype
        )
  (:export #:def-column-struct
           #:column-struct-size
           #:column-struct-last-index
           #:column-struct-reset
           #:column-struct-resize))
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
  (slot-names   nil :type list       :read-only t)
  (fill-pointer   0 :type sb-vm:word)
  (batch-size   nil :type (or null
                              (integer 1 1000000000))
                :read-only t))

(defstruct (udef-c-s-lower
             (:conc-name csl-))
  (master-var   nil :type symbol     :read-only t)
  (batch-index    0 :type fixnum)
  (fill-pointer   0 :type sb-vm:word))

(defstruct (udef-c-s-upper
             (:include udef-c-s-metadata)
             (:conc-name csu-))
  (lock (sb-thread:make-mutex :name "c-s-upper-lock")
        :type sb-thread:mutex
        :read-only t)
  (make-lower nil :type function)
  ;; lower vector slot added dynamically because of dynamic type
  )

(defstruct (udef-c-s-only
             (:include udef-c-s-metadata)
             (:conc-name cso-))
  ;; Other slots added dynamically
  )

(defun get-upper-last-batch (obj)
  (let* ((lower (slot-value obj 'lower))
         (last-batch-nr (1- (length lower))))
    (aref lower last-batch-nr)))

(declaim (inline get-new-id-range))
(defun get-new-id-range (obj)
  (sb-ext:atomic-incf (c-s-fill-pointer obj)))

(defun column-struct-reset (obj)
  "Soft-resets OBJ, ie. sets the last used index to 0."
  (cond
    ((symbolp obj)
     (let ((data (or (get obj 'column-struct-data)
                     (error "~s is not a column-structure type." obj))))
       (column-struct-reset (symbol-value data))))
    ((typep obj 'udef-c-s-only)
     (setf (cso-fill-pointer obj) 0))
    ((typep obj 'udef-c-s-lower)
     (setf (csl-fill-pointer obj) 0))
    ((typep obj 'udef-c-s-upper)
     (map 'nil #'column-struct-reset
          (slot-value obj 'lower)))
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
                                    (c-s-slot-names obj)))
                      0))
    ((typep obj 'udef-c-s-upper)
     (* (length (slot-value obj 'lower))
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
    ((or (typep obj 'udef-c-s-only)
         (typep obj 'udef-c-s-upper))
     (c-s-fill-pointer obj))
     #+(or)
    ((typep obj 'udef-c-s-upper)
     (let* ((last-batch (get-upper-last-batch obj)))
       (+ (* (csl-batch-index last-batch)
             (csu-batch-size obj))
          (csl-fill-pointer last-batch))))
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
     (dolist (slot (c-s-slot-names obj))
       (let ((old (slot-value obj slot)))
         (setf (slot-value obj slot)
               (adjust-array old new-size
                             ;;:initial-element ??
                             :element-type (array-element-type old)))))
     (column-struct-size obj))
    ;;
    ((typep obj 'udef-c-s-upper)
     (let* ((batches-wanted (ceiling new-size (c-s-batch-size obj)))
            (old (slot-value obj 'lower)))
       ;; TODO: also make smaller?
       ;; Don't use ARRAY-DIMENSION, the array is ADJUSTABLE
       (loop for size-now = (length old)
             while (< size-now batches-wanted)
             ;do (describe old *trace-output*)
             for new-lower-index = (vector-push-extend
                                     (funcall (csu-make-lower obj)
                                              :master-var (csu-master-var obj)
                                              :batch-index size-now)
                                     old)
             ;;do (format *trace-output* "resize from ~d, new lower ~d, ~s~%"
             ;;           size-now new-lower-index
             ;;           (map 'list #'sb-kernel:get-lisp-obj-address old)
             ;;           #+(or)
             ;;           (map 'list (lambda (l)
             ;;                        (length (slot-value l (first (csu-slot-names obj)))))
             ;;                old))
             ))
     (column-struct-size obj))
    ;;
    (t
     (error "Bad type for ~s" obj)))
  ;(format *trace-output* "resized to ~d~&" (column-struct-size obj))
  (column-struct-size obj)
  )


(defun c-s-value% (2-level? data-var slot-name idx &optional (before 'identity) after)
  (if 2-level?
      (sb-int:with-unique-names (batch i lower)
        `(multiple-value-bind (,batch ,i) (floor ,idx (c-s-batch-size ,data-var))
           (let ((,lower (slot-value ,data-var 'lower)))
             (unless (< ,batch (array-dimension ,lower 0))
               (error "Data ~s out of bounds" ,idx))
             (,before (aref (slot-value (aref ,lower ,batch)
                                        ',slot-name)
                            ,i)
                      ,@ after))))
      `(,before (aref (slot-value ,data-var
                                  ',slot-name)
                      ,idx)
                ,@ after)))


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
               (upper-struct (gensym (format nil "~a-~a" :col-struct-0 struct-name)))
               (data-var (option :data-var (gensym (format nil "~a-~a" :data struct-name))))
               (base-constructor (gensym (format nil "*~a-CONSTRUCTOR*" struct-name)))
               ;;
               ;;
               (slot-names ())
               (my-slots ())
               (constructor-arg-list ())
               (accessor-names ())
               (fns ()))
          (when (zerop (length slots))
            (error "Need at least one slot in ~s" struct-name))
          ;; TODO: pull ,init out into a LET per slot?
          (dolist (slot slots)
            (destructuring-bind (name &optional init &key (type t))
                (if (consp slot)
                    slot
                    (list slot))
              (let ((accessor (intern (format nil "~a-~a" struct-name name)
                                      (symbol-package struct-name))))
                (push name slot-names)
                (push `(,name (make-array (list ,(or batch-size initial-size))
                                                    :initial-element ,init
                                                    :element-type ',type)
                              :type (simple-array ,type (,(or batch-size '*))))
                      my-slots)
                (push `(,name ,init) constructor-arg-list)
                (push accessor accessor-names)
                ;;
                (push `(defun ,accessor (id)
                         (declare (optimize (speed 3) (safety 1) (debug 1))
                                  (type ,struct-name id))
                         (let ((idx (,udef-reader id)))
                           ,(c-s-value% 2-level? data-var name 'idx)))
                      fns)
                (push `(defun (setf ,accessor) (new-val id)
                         (declare (optimize (speed 3) (safety 1) (debug 1))
                                  (type ,struct-name id))
                         (let ((idx (,udef-reader id)))
                           ,(c-s-value% 2-level? data-var name 'idx
                                       'setf '(new-val))))
                      fns))))
          ;; only once, and before everything else
          ;; TODO: a (sb-ext:defglobal) in the macro code below?
          (let ((base-defaults `((master-var ',data-var)
                                 (batch-size ,batch-size)
                                 (slot-names ',(reverse slot-names))
                                 (udef ',struct-name))))
            (cond
              (2-level?
               (eval `(defstruct (,col-struct
                                 (:constructor ,ll-constructor)
                                 (:include udef-c-s-lower
                                  (master-var ',data-var)))
                      ,@ my-slots))
               (eval `(defstruct (,upper-struct
                                 (:constructor ,var-constructor)
                                 (:include udef-c-s-upper
                                  (make-lower #',ll-constructor)
                                  ,@ base-defaults))
                      (lower (make-array 1 :element-type ',col-struct
                                         :adjustable t
                                         :initial-contents (list (,ll-constructor))
                                         :fill-pointer 0)))))
              (t ;; single-level only
               (setf upper-struct col-struct)
               (eval `(defstruct (,col-struct
                                 (:constructor ,var-constructor)
                                 (:include udef-c-s-only
                                  ,@ base-defaults))
                      ,@ my-slots)))))
          (sb-ext:with-current-source-form (options slots)
            `(progn
               (declaim (type (or null ,upper-struct) ,data-var))
               (defparameter ,data-var nil)
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
             (declaim (inline ,base-constructor))
             (defun ,base-constructor (,where ,idx ,@ (reverse slot-names))
               ;; TODO: provide restart for reallocation
               (let ((,old-size (column-struct-size ,where)))
                 (when (>= ,idx ,old-size)
                   (column-struct-resize ,where
                                         (max (floor (* 1.41 ,old-size))
                                              (+ ,old-size
                                                 ,(or batch-size 50))))))
               ;; Make setup order same as slot order
               ,@ (nreverse
                    (mapcar
                      (lambda (name)
                        (c-s-value% 2-level? data-var name idx 'setf (list name)))
                      slot-names))
               ;; Return (doubly-)tagged UDEF-INTTYPE
               (,udef-maker ,idx))
             ;; This arglist must be in correct order, though
             (defun ,constructor-name (&key ,@ (nreverse constructor-arg-list))
               ;; Value before incrementing
               (let ((,idx (get-new-id-range ,data-var)))
                 (,base-constructor ,data-var ,idx ,@ (reverse slot-names))))
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
          ,@ fns
          ;;
          ;; BROKEN: There is no class named X.
          #+(or)
          (defmethod sb-c::describe-object :after ((obj ,struct-name) stream)
            ,@(loop for acc in (reverse accessor-names)
                    for slot in (reverse slot-names)
                    collect `(format stream "~&  ~A = ~A~%" ',slot (,acc obj))))
          ;;
          (setf ,data-var (,var-constructor))
          (column-struct-reset ,data-var)
          (column-struct-resize ,data-var ,initial-size)
          ',struct-name)))))))

;; TODO: don't EVAL, but return one form using EVAL-WHEN?
;; TODO: box/unbox into (unsigned-byte X) specialized arrays and slots
;; TODO: optionally a freelist
