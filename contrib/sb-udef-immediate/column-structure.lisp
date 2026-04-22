;;; Define a column-structure.
;;
;; The structure name is used to create a UDEF-IMMEDIATE,
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
(in-package :sb-udef-immediate)

;(declaim  (OPTIMIZE SB-C::INSTRUMENT-CONSING))

;; Sizing heuristics for parallel allocation in threads
(defun default-batch-size (guess)
  ;; Power-of-two of about 1/4 the initial size
  (expt 2
        (ceiling
          (log (* 0.25 guess)
               2))))

(defstruct (udef-c-s-metadata
             (:conc-name cs-meta-))
  (udef            nil :type symbol          :read-only t)
  (next              0 :type sb-vm:word)
  (allocated         0 :type sb-vm:word)
  (index-bits      nil :type (or null (integer 1 48)))
  (as-alist        nil :type symbol          :read-only t)
  (p-function      nil :type symbol          :read-only t)
  (batch-size      nil :type (or null
                                 ;; should actually start at 1000 or so,
                                 ;; but race tests run with small increments
                                 (integer 3 1000000000))
                   ;; Allow modification during test
                       :read-only nil)
  (lock            (sb-thread:make-mutex :name "c-s-upper-lock")
                       :type sb-thread:mutex
                       :read-only t)
  (slots           nil :type vector           :read-only t)
  (constructor     nil :type symbol           :read-only t)
  ;; Slot defining the length for variable-length slots
  (var-len-slot-name nil :type symbol               :read-only t)
  (var-len-slot    nil :type t               :read-only t)
  (has-index-slot  nil :type (member nil t ) :read-only t)
  (data-vec        #() :type simple-vector   :read-only t))


;; ------------------------------------------------------------

(defstruct (cs-slot
             (:type list)
             :named)
  (slot-name     nil :type symbol)
  (allocation    nil :type symbol)
  (accessor-sym  nil :type symbol)
  (init-value    nil :type t)
  (init-fn       nil :type symbol)
  (new-spec      nil :type t)
  (orig-type     nil :type t))

;; ------------------------------------------------------------

(defvar *bit-index* nil
  "Used to count used immediate bits during parsing")
(defvar *storage-symbol* nil
  "Shortcut to the data vector.")
(defvar *current-cs-sym* nil
  "The symbol we're defining a C-S on.")
(defvar *oper-func* nil
  "The symbol for the udef operator.")
(defvar *p-func-sym* nil
  "The symbol for the predicate.")
(defvar *batch-size* nil
  "How many items to allocate at once in a 2nd level vector.")
(defvar *vector-storage-index* nil
  "Used to number the slots during parsing")
(defvar *slot-nr* nil
  "Used to number the slots during parsing")
(defvar *index-bits* nil
  "The number of _index_ bits for the current C-S.")
(defvar *max-bits* nil
  "The total size for the current C-S.")
(defvar *index-slot* nil
  "The index slot definition, if any.")

;; ------------------------------------------------------------

(defun get-udef-metadata-from-symbol (sym)
  (let ((v (and
             (symbolp sym)
             (get sym 'udef-metadata))))
    (when v
      (the udef-metadata v))))

(declaim (inline get-cs-metadata-from-symbol))
(defun get-cs-metadata-from-symbol (sym &optional c-s-req)
  (cond
    ((symbolp sym)
     (let ((v-c-s (get sym 'column-struct-data)))
       (if (and c-s-req
                (not v-c-s))
           (error "~s is not a column-structure type." sym)
           (values (and v-c-s
                        (the udef-c-s-metadata v-c-s))
                   (get-udef-metadata-from-symbol sym)))))
    ((udef-c-s-metadata-p sym)
     (values sym
             (get-udef-metadata-from-symbol (cs-meta-udef sym))))))

;; ------------------------------------------------------------

(defun is-unsigned-byte-slot? (slot)
  "Returns the number of bits for an (UNSIGNED-BYTE x) type."
  (let ((type (cs-slot-orig-type slot)))
    (when (and (consp type)
               (eq (first type) 'unsigned-byte)
               (integerp (second type))
               (null (cddr type)))
      (second type))))

(defun is-udef-slot? (slot)
  "Returns the UDEF metadata or NIL."
  (let ((type (cs-slot-orig-type slot)))
    (multiple-value-bind (len vtype) (slot-is-vector-type slot)
      (declare (ignore len))
      (or (get-udef-metadata-from-symbol type)
          (get-udef-metadata-from-symbol vtype)))))

(defun is-immediate-slot? (slot)
  (eq (cs-slot-allocation slot)
      :immediate))

(defun slot-input-type (slot)
  (multiple-value-bind (vector-length vtype) (slot-is-vector-type slot)
    (declare (ignore vector-length))
    (let ((type (or vtype
                    (cs-slot-orig-type slot))))
      (if (is-udef-slot? slot)
          `(or null ,type)
          type))))

(defun udef-storage-bits-needed (udef)
  (let ((udef (ensure-udef udef)))
    (unless udef
      (error "Expected ~s to be a UDEF" udef))
    (let ((current? (eq *current-cs-sym*
                        (udef-metadata-udef-sym udef))))
      ;; Need to check -- if it's the current C-S, we need :index-bits specified.
      ;; Else just take the total udef length.
      (or (when current?
            *index-bits*)
          (when udef
            ;; we need the total size, including any other immediate data!
            (udef-metadata-max-bits udef))
          (error "For self-referencing column-structures (type ~s) ~
                 you need to specify :INDEX-BITS."
                 (udef-metadata-udef-sym udef))))))

(defun slot-byte-length (slot)
  (let ((udef (is-udef-slot? slot)))
    (if udef
        (udef-storage-bits-needed udef)
        (is-unsigned-byte-slot? slot))))

(defun immediate-slot-byte-spec (slot)
  (when (is-immediate-slot? slot)
    (let ((len (slot-byte-length slot)))
      (when len
        `(byte ,len ,*bit-index*)))))

(defun slot-is-vector-type (slot)
  "Returns NIL or the vector length and the element type."
  (let ((type (cs-slot-orig-type slot)))
    (when (and (listp type)
               (= 3 (length type)))
      ;; restricted to a subset of possible array-types
      (destructuring-bind (kind element-type dims) type
        (when (and (member kind '(array simple-array vector))
                   (= 1 (length dims)))
          (values (first dims)
                  element-type))))))

(defun is-udef-vector-slot? (slot)
  "Returns the UDEF metadata or NIL."
  (multiple-value-bind (dim el-type) (slot-is-vector-type slot)
    (declare (ignore dim))
    (and el-type
         (get-udef-metadata-from-symbol el-type))))

(defun slot-storage-type (slot)
  (let ((bits (slot-byte-length slot)))
    (multiple-value-bind (dim el-type) (slot-is-vector-type slot)
      (cond
        ((is-immediate-slot? slot)
         nil)
        (bits
         `(unsigned-byte ,bits))
        (dim
         el-type)
        (t
         (cs-slot-orig-type slot))))))

(defun make-array-with-slot-storage-type (slot len &rest args)
  `(make-array ,len
               :element-type ',(slot-storage-type slot)
               ,@ args))

(defun slot-is-fixed-vector-type (slot)
  "Returns NIL NIL or the constant vector length and the element type."
  (multiple-value-bind (dimension element-type) (slot-is-vector-type slot)
    (when (numberp dimension)
      (values dimension element-type))))

(defun vec-slot-index-multiplier (slot)
  (let ((dim (slot-is-fixed-vector-type slot)))
    (or dim
        1)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun find-slot-by-name (c-s slot-name)
  (find slot-name
        (if (or (listp c-s)
                (vectorp c-s))
            c-s
            (cs-meta-slots (if (symbolp c-s)
                               (get-cs-metadata-from-symbol c-s)
                               c-s)))
        :test #'eq
        :key #'cs-slot-slot-name))

(defun ensure-udef (udef)
  (cond
    ((udef-metadata-p udef)
     udef)
    ((eq *current-cs-sym* udef)
     (get-udef-metadata-from-symbol udef)
     #+(or)(error "yyy"))
    ((symbolp udef)
     (get-udef-metadata-from-symbol udef))))


(defun convert-from-udef-if-needed (udef? form)
  (let ((ud (ensure-udef udef?)))
    (if ud
        `(,(udef-metadata-func ud) :udef-or-nil-to-ub-x ,form)
        form)))

(defun convert-to-udef-if-needed (udef? form)
  (let ((ud (ensure-udef udef?)))
    (if ud
        `(,(udef-metadata-func ud) :ub-x-to-udef-or-nil ,form)
        form)))

(defun get-storing-form (slot var-name)
  (let ((udef (is-udef-slot? slot))
        (len (slot-byte-length slot)))
    (cond
      (udef
       (assert len)
       `(the (unsigned-byte ,len)
             ,(convert-from-udef-if-needed udef var-name)))
      (t
       var-name))))

(defun make-udef-with-immediate-slots-form (slots &optional (start-bit 0))
  `(,*oper-func*
     :int-to-tagged-udef
     (logior
       ,@ (loop for slot in slots
                when (is-immediate-slot? slot)
                ;; The argument is named from the slot name.
                collect `(ash ,(get-storing-form slot
                                                 (cs-slot-slot-name slot))
                              ,start-bit)
                and do (incf start-bit (slot-byte-length slot))))))

(defun parse-slot (input)
  (declare (sb-ext:muffle-conditions style-warning))
  (destructuring-bind (name &optional init &key (type t) (allocation :default))
      (sb-int:ensure-list input)
    (values name init type allocation)))

(defun make-slot-def (input)
  (multiple-value-bind (name init user-type allocation) (parse-slot input)
    (let ((slot (make-cs-slot :slot-name name
                              :allocation allocation
                              :accessor-sym (sb-int:symbolicate *current-cs-sym* :- name)
                              :orig-type user-type
                              :init-value init
                              :init-fn (sb-int:gensymify* *current-cs-sym* :-slot-init- name))))
      #+(or)
      (unless (is-immediate-slot? slot)
        (setf (cs-slot-new-spec slot)
              (produce-new-spec-for-slot slot)))
      slot)))

(defun produce-immediate-accessors (slot)
  (let ((udef (is-udef-slot? slot))
        (accessor-sym (cs-slot-accessor-sym slot)))
    `((defun ,accessor-sym (id)
        (declare (optimize (speed 3) (safety 1) (debug 1))
                 #+(or)
                 (type ,*current-cs-sym* id))
        ,(convert-from-udef-if-needed udef
                                      `(ldb ,(immediate-slot-byte-spec slot)
                                            (sb-kernel:get-lisp-obj-address id))))
      (defun (setf ,accessor-sym) (new place immediate!)
        (declare (optimize (speed 3) (safety 1) (debug 1))
                 (type ,(slot-input-type slot) new)
                 #+(or)
                 (type ,*current-cs-sym* place))
        ;; No setter function for immediate slots -
        ;; they're used to _address_ data and so can't be changed afterwards??!
        (unless (eq immediate! :immediate)
          (error "Immediate column-structure slots are passed by value, please acknowledge this fact"))
        (let ((raw (sb-kernel:get-lisp-obj-address place)))
          (setf (ldb ,(immediate-slot-byte-spec slot) raw)
                ,(get-storing-form slot 'new))
          (setf place
                (sb-kernel:%make-lisp-obj raw)))
        new))))

(defun setup-slot-vector-addressing (slot input storage-vec vec-index-sym body)
  ;c-s slot input vec-name vec-index-sym body-form)
  (assert *index-slot*)
  (let ((stor-type (slot-storage-type slot)))
    (sb-int:with-unique-names (iidx input-index batch-idx inner-idx 2nd-or-storage-vec)
      `(let* ((,iidx ,input)
              (,input-index (cond ((, *p-func-sym* ,iidx)
                                   (,(cs-slot-accessor-sym *index-slot*)
                                     ,iidx))
                                  ;; TODO: allow an index as well?
                                  (t
                                   (error "wrong input ~s, expect udef ~s"
                                          ,iidx ',*current-cs-sym*)))))
         (multiple-value-bind (,batch-idx ,inner-idx)
             ;; TODO: batch-size changeable during runtime?
             ,(if *batch-size*
                  `(floor ,input-index ,*batch-size*)
                  `(values nil ,input-index))
           ,(unless *batch-size*
              `(declare (ignore ,batch-idx)))
           (let* ((,2nd-or-storage-vec (svref ,*storage-symbol* ,*vector-storage-index*))
                  (,storage-vec ,(if *batch-size*
                                     `(aref (the (simple-array simple-array (*))
                                                 ,2nd-or-storage-vec)
                                            ,batch-idx)
                                     2nd-or-storage-vec))
                  (,vec-index-sym (* ,inner-idx
                                     ,(vec-slot-index-multiplier slot))))
             ;(declare (ignorable ,storage-vec))
                                     ;; Use *batch-size* here?
             (declare (type (simple-array ,stor-type (*)) ,storage-vec))
             , body))))))

(defun produce-vector-data-forms (slot mode-sym vec-sym
                                       start-sym end-sym len-sym)
  (let ((udef (is-udef-slot? slot)))
    `(ecase ,mode-sym
       (:displaced
        (values
          ,(if udef
               ;; TODO: How to get changes reflected back into the original
               ;; if there's a translation, eg. for an UDEF??
               ;`(make-array len
               ;               :element-type ',(cs-s-array-el-type slot)
               ;               :initial-contents
               ;               (map 'list
               ;                    (lambda (x)
               ;                      (,func :ub-x-to-udef-or-nil x))
               ;                    ,backend-data))
               `(error "Can't return a displaced vector, ~
                       translation storage type ~s / visible type ~s is needed for slot ~s."
                       ',(slot-storage-type slot)
                       ',(cs-slot-orig-type slot)
                       ',(cs-slot-slot-name slot))
               (make-array-with-slot-storage-type
                 slot len-sym
                 :displaced-to vec-sym
                 :displaced-index-offset start-sym))))
       (:subseq
        (values
          ,(let ((backend-data `(subseq ,vec-sym ,start-sym ,end-sym)))
             (if udef
                 ; use len-sym here?
                 ; WARNING: bad dimension in array type: LEN
                 `(map '(array (or null
                                   ,(udef-metadata-udef-sym udef))
                               (*))
                       (lambda (x)
                         ,(convert-to-udef-if-needed udef `x))
                       ,backend-data)
                 backend-data)))))))

(defun produce-slice-accessors (slot)
  (let* ((accessor-sym (cs-slot-accessor-sym slot))
         (udef (is-udef-slot? slot))
         (default-mode (if udef :subseq :displaced)))
    (multiple-value-bind (dimension el-type) (slot-is-vector-type slot)
      (assert (and dimension el-type))
      (flet
          ((access-body (body)
             `((declare (type (or null sb-int:index) index)
                        (type (or null sb-int:index) start)
                        (type (or null sb-int:index) end))
               ,(setup-slot-vector-addressing
                  slot 'id 'vec 'base-idx
                  `(let ((max-len ,(if (numberp dimension)
                                       dimension
                                       ;; Retrieve
                                       `(with-c-s-slots (,*current-cs-sym* id) (,dimension)
                                          ,dimension))))
                     (declare (type sb-int:index max-len))
                     (check-type index (or null sb-int:index))
                     (when index
                       (assert (<= index max-len)))
                     (check-type start (or null sb-int:index))
                     (when start
                       (assert (<= start max-len)))
                     (check-type end   (or null sb-int:index))
                     (when end
                       (assert (<= (or start 0) end max-len)))
                     , body)))))
        `((defun ,accessor-sym (id &key start end index (mode ,default-mode))
            "Use :INDEX for a single element, :START and :END for a slice,~
            and :MODE :SUBSEQ for a separate sequence, or :DISPLACED."
            (declare (optimize (speed 3) (safety 1) (debug 1))
                     #+(or)
                     (type ,*current-cs-sym* id))
            ,@(access-body
                `(if index
                   (values ,(convert-to-udef-if-needed
                              udef
                              `(aref vec (+ base-idx index))))
                     (let* ((real-start (+ base-idx (or start 0)))
                            (real-end (+ base-idx (or end max-len)))
                            (len (- real-end real-start)))
                       (declare (ignorable len))
                       ,(produce-vector-data-forms slot 'mode
                                                   'vec
                                                   'real-start 'real-end 'len)))))
          (defun (setf ,accessor-sym) (new id &key start end index)
            (declare (optimize (speed 3) (safety 1) (debug 1))
                     #+(or)
                     (type ,*current-cs-sym* id))
            ,@(access-body
                `(progn
                   (if index
                     (setf (aref vec (+ index base-idx))
                           ,(convert-from-udef-if-needed el-type 'new))
                     (let* ((real-start (+ base-idx (or start 0)))
                            (real-end (+ base-idx (or end max-len))))
                       ; #(6 4 1) is not of type (VECTOR (UNSIGNED-BYTE 32)).
                       ; We need to be lax -- but how much type-checking is right?
                       ;(check-type new (array ,(slot-input-type slot) (*)))
                       ;(assert (typep new '(array ,(slot-input-type slot) (*))))
                       (loop for el across new
                             for i upfrom real-start below real-end
                             do (setf (aref vec i)
                                      ,(convert-from-udef-if-needed el-type `el)))))
                   new))))))))

(defun produce-atom-accessors (slot)
  ;(defmethod getter-and-setter-functions ((slot cs-mixin-vector-storage-slot) cs)
  ;(multiple-value-bind (func) (possibly-udef-value-translation slot)
  (let ((udef (is-udef-slot? slot))
        (accessor-sym (cs-slot-accessor-sym slot)))
    `((defun ,accessor-sym (id)
        (declare (optimize (speed 3) (safety 1) (debug 1))
                 #+(or) (type ,*current-cs-sym* id))
        ,(setup-slot-vector-addressing
           slot 'id 'vec 'idx
           (convert-to-udef-if-needed udef
                                      `(aref vec idx))))
      (defun (setf ,accessor-sym) (new id)
        (declare (optimize (speed 3) (safety 1) (debug 1))
                 (type ,(slot-input-type slot) new)
                 #+(or) (type ,*current-cs-sym* id))
        ,(setup-slot-vector-addressing
           slot 'id 'vec 'idx
           `(setf (aref vec idx)
                  ,(get-storing-form slot 'new)))
        new)
      ;; Provide ATOMIC-PUSH etc.
      (defun (sb-ext:cas ,accessor-sym) (old new id)
        ,(setup-slot-vector-addressing
           slot 'id 'vec 'idx
           ;; can't make it work with SB-EXT:CAS,
           ;; UNDEFINED-FUNCTION always gets compiled in
           ;`(sb-c::%compare-and-swap-svref vec idx old new)
           `(sb-ext:cas (aref vec idx) old new)
           )))))

(defun produce-vector-storage-accessors (slot)
  (if (slot-is-vector-type slot)
      (produce-slice-accessors slot)
      (produce-atom-accessors slot)))


(defun produce-accessors (slots)
  (let ((*bit-index* +udef-reserved-low-bits+)
        (*vector-storage-index* 0))
    (loop for slot in slots
          for funs = (if (is-immediate-slot? slot)
                         (prog1
                             (produce-immediate-accessors slot)
                           (incf *bit-index* (slot-byte-length slot)))
                         (prog1
                             (produce-vector-storage-accessors slot)
                           (incf *vector-storage-index*)))
          append funs)))

(defun slot-argument-init-from-udef (slot)
  (convert-from-udef-if-needed
    (is-udef-slot? slot)
    (cs-slot-init-value slot)))

(defun produce-vector-argument (slot)
  (let ((vector-length (slot-is-vector-type slot)))
    (list (cs-slot-slot-name slot)
          `(make-array (list ,vector-length)
                       :initial-element ,(cs-slot-init-value slot)
                       ;:element-type ',(slot-storage-type slot)
                       :element-type ',(slot-input-type slot)))))

(defun produce-nonvector-argument (slot)
  (list (cs-slot-slot-name slot)
              (cs-slot-init-value slot)))

(defun produce-argument-list (slots)
  (loop for slot in (coerce slots 'list)
        for vector-length = (slot-is-vector-type slot)
        ;for init = (slot-argument-init-from-udef slot)
        ;; Any UDEF => (unsigned X) conversion happens in the SETF.
        collect (if vector-length
                    (produce-vector-argument slot)
                    (produce-nonvector-argument slot))))

(defun fixup-bits (needs-index-slot immediate-bits)
  (cond
    ((and needs-index-slot (not *index-bits*)      *max-bits*)
     (setf *index-bits*         (- *max-bits* immediate-bits)))
    ((and needs-index-slot (not *index-bits*) (not *max-bits*))
     (setf *max-bits*           +udef-usable-remaining-bits+
           *index-bits*         (- *max-bits* immediate-bits)))
    ((and needs-index-slot)
     (assert *index-bits*)))
  ;; index slot handled, now check/do *max-bits*
  (cond
    ((and      *index-bits*       *max-bits*)
     t)
    ((and      *index-bits*  (not *max-bits*))
     (setf *max-bits*           (+ *index-bits* immediate-bits)))
    ((and (not *index-bits*)      *max-bits*)
     (setf *index-bits*         (- *max-bits* immediate-bits)))
    ((and (not *index-bits*) (not *max-bits*))
     (setf *max-bits*           +udef-usable-remaining-bits+
           *index-bits*         (- *max-bits* immediate-bits))))
  )

(defun produce-constructors (internal visible slots var-len index-slot)
  (let ((slot-names (mapcar #'cs-slot-slot-name slots))
        (has-stored-slots? (remove #'is-immediate-slot? slots))
        (maker-arg-list (produce-argument-list slots)))
    (sb-int:with-unique-names (tmp numeric-index
                                   len up-to c-s
                                   old-size )
      `((declaim (inline , internal))
        ;; First slot is the immediate index, to be returned (with other immediates) in a udef!
        (defun ,internal ,slot-names
          (declare (optimize (debug 3)
                             (speed 1)
                             (compilation-speed 0)
                             (safety 1)))
          (let* ((,c-s (get-cs-metadata-from-symbol ', *current-cs-sym*)))
            (declare (notinline column-struct-size)
                     #+(or)(type udef-c-s-metadata ,c-s))
            ;; drop the resize checks for immediate-only structures
            ,(when (and index-slot
                         has-stored-slots?)
                ;; Make numeric index available via correct name
                `(let* ((,len (or ,var-len 1))
                        ;; For a variable-length allocation, we need that much free space
                        (,up-to (+ ,(cs-slot-slot-name index-slot) ,len)))
                   (loop for ,old-size = (column-struct-size ,c-s)
                         while (>= ,up-to ,old-size)
                         for i from 0 below 5
                         do (column-struct-resize
                              ,c-s
                              ;; 1 level: exponential growth;
                              ;; 2-level: only one (batch-size) element added
                              ,(if *batch-size*
                                   `(+ ,up-to
                                       ,*batch-size*) ;; TODO: allow later changing?
                                   `(max (round (* (sqrt 2)
                                                   ,up-to))
                                         (+ ,up-to
                                            50)))))
                   (when (>= ,up-to (column-struct-size ,c-s))
                     (error "Cannot resize ~s" ',*current-cs-sym*))))
            ;;
            (let ((,tmp ,(make-udef-with-immediate-slots-form slots 0)))
              ;; Generate SETFs for column-struct slots
              ,@(loop for slot in slots
                      for name = (cs-slot-slot-name slot)
                      unless (is-immediate-slot? slot)
                      collect `(setf (,(cs-slot-accessor-sym slot) ,tmp)
                                     ,name) into setfs
                      finally (return setfs))
              ;; Return specifically tagged UDEF-IMMEDIATE
              ,tmp)))
        ;;
        ;; User-visible constructor function.
        ,(if *index-slot*
             `(defun , visible (&key ,@ (rest maker-arg-list))
                ;; ,@ (rest maker-checks)
                ;; Value before incrementing
                ;; Asked from the _current_ metadata in case of COLUMN-STRUCT-RESET
                (let* ((,tmp (get-cs-metadata-from-symbol ',*current-cs-sym*))
                       (,numeric-index (get-new-id-range ,tmp
                                                         ,var-len
                                                         t))
                       (max ,(expt 2 *index-bits*)))
                  ;; TODO: -1 because of nil-value?
                  (when (>= ,numeric-index max)
                    (error "Overflowing UDEF index ~s: #x~x"
                           ',*current-cs-sym* max))
                  ;; First arg is the numeric index!
                  (, internal ,numeric-index
                     ,@ (rest slot-names))))
             `(defun , visible (&key ,@ maker-arg-list)
                ;; ,@ maker-checks
                (, internal ,@ slot-names)))))))

(declaim (inline get-new-id-range))
(defun get-new-id-range (obj &optional count one-linear-piece?)
  "Returns the _old_ value, before incrementing"
  (let* ((count (or count 1)))
    (check-type count (integer 0 #. (floor most-positive-fixnum 4)))
    (cond
       ;; Single-level allocation or one element only.
       ;; Maybe needs resizing; the constructor will do that.
      ((or (= count 1)
           (not (cs-meta-batch-size obj))
           (not one-linear-piece?))
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
         (let* ((cur (the (integer 0 #. (floor most-positive-fixnum 2))
                          (cs-meta-next obj)))
                (bs (cs-meta-batch-size obj))
                (position-within-batch (mod cur bs))
                (next-pos (+ cur count)))
           (if (<= (+ position-within-batch count) bs)
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
                                     bs))
                      (allocation-end (+ next-batch count)))
                 ;; Try to get that position
                 (let ((chg (sb-ext:compare-and-swap (cs-meta-next obj)
                                                     cur
                                                     allocation-end)))
                   (when (= chg cur)
                     (return-from get-new-id-range next-batch)))))))
       (error "can't get new allocation for ~s even after a few retries. broken logic?"
              (cs-meta-udef obj))))))


(defmacro with-batched-cs-allocation ((c-s-sym fn-name &key batch-size new-batch-cb) &body body)
  "Provide FN-NAME around BODY as a local constructor,
  taking the same arguments as the constructor of C-S-SYM,
  with a locally reserved range of IDs so that threads operate
  on different cache lines.
  NEW-BATCH-CB, when given, gets called on new reservations
  with the new base index and the length."
  (check-type fn-name symbol)
  (sb-int:with-unique-names (next left new-batch data-var)
    ; (defun return-batch-macro (macro-name constructor slots)
    (let* ((c-s (get-cs-metadata-from-symbol c-s-sym t))
           (arg-list (produce-argument-list (cs-meta-slots c-s))))
      `(let ((,next 0)
             (,left 0)
             (,data-var (get-cs-metadata-from-symbol ',c-s)))
         (labels
             ((,new-batch ()
                ;; TODO: try to exactly fill up, so that other threads
                ;; or the next calls have clean batches just for themselves?
                (setf ,left (or ,batch-size (cs-meta-batch-size ,data-var)))
                (setf ,next (get-new-id-range ,data-var ,left))
                ,(when new-batch-cb
                  `(funcall ,new-batch-cb ,next ,left)))
              ;; Index gets calculated
              (,fn-name (&key ,@ (rest arg-list))
                (let* ((id ,next)
                       (needed (or ,(cs-meta-var-len-slot-name c-s)
                                   1)))
                  (when (/= 1 needed)
                    (error "allocations > 1 element via batch macro not implemented yet"))
                  (when (< ,left needed)
                    (,new-batch)
                    (setf id ,next))
                  (incf ,next)
                  (decf ,left)
                  (,(cs-meta-constructor c-s)
                    id
                    ,@ (rest
                         (map 'list
                              #'cs-slot-slot-name
                              (cs-meta-slots c-s)))))))
           (locally
             ,@ body))))))

(defmacro def-column-struct (name-and-options &rest slot-definitions)
  "Like DEFSTRUCT, but creates a (or, for :BATCH-SIZE, multiple) array(s) per slot,
  and the \"identity\" of an instance is actually the index
  in the arrays, returned as a user-defined integer type.

  Redefinition looses all old data (like with DEFSTRUCT,
  though for different reasons.).

  Available options:
  - :CONSTRUCTOR (defaults to MAKE-<name>)
  - :INDEX-BITS resp. :MAX-BITS
  - :INITIAL-SIZE to avoid initial reallocations
  - :BATCH-SIZE allocation size for a 2-level structure;
    pass T for a heuristic to derive a size from :INITIAL-SIZE
  - :WITH-BATCH-MACRO to provide a macro that does batch allocations,
    for use in heavy multi-threaded programs

  Advantages:
  - GC only sees a few big specialized vectors, so does much less work
  - smaller instance \"pointers\" (depending on the number of items
    eg. 32bit or even smaller) when referencing from other column-structures
  Restrictions:
  - Garbage collection is not available -- only reset of all data,
    or you need to keep a free list or free pointer chain."
  ;; GC is not technically impossible, only TODO
  (declare (optimize (speed 0) (debug 3)))
  (destructuring-bind (struct-name &rest options) (sb-int:ensure-list name-and-options)
    (assert (symbolp struct-name))
    (flet ((option (name default)
             (or (second (find name options :key #'first)) default)))
      (let* ((*current-cs-sym* struct-name)
             (*bit-index* +udef-reserved-low-bits+)
             (*slot-nr* 0)
             (initial-size (option :initial-size 16384))
             (batched% (option :batch-size nil))
             (*batch-size* (if (eq batched% t)
                               (default-batch-size initial-size)
                               batched%))
             ;;
             (index-name (sb-int:gensymify* :%index))
             ;; Default to UDEF max-bits?
             ;; These depend on each other and any :IMMEDIATE slots -
             ;; and self-referencing slots need to know how many bits they need...
             (index-bits (option :index-bits nil))
             (*index-bits* index-bits)
             ;;
             (existing-udef (get-udef-metadata-from-symbol *current-cs-sym*))
             (wants-nil (or (option :reserve-nil-value nil)
                              (and existing-udef
                                   (sb-udef-immediate::udef-metadata-nil? existing-udef))))
             (*oper-func* (or (option :udef-operator nil)
                              (and existing-udef
                                   (sb-udef-immediate:udef-metadata-func existing-udef))
                              (sb-int:symbolicate :udef/operator- struct-name)))
             (%%1 (setup-udef struct-name
                              :func% *oper-func*
                              :udef-sym struct-name
                              :max-bits *index-bits*
                              :max-bits nil))
             ;;
             (*max-bits* (option :max-bits nil))
             (*storage-symbol* (sb-int:gensymify* struct-name :-storage))
             ;;
             (base-constructor (option :base-constructor (sb-int:gensymify* struct-name :-constructor)))
             ;;
             (constructor-name (option :constructor
                                       (intern (format nil "~a~a" :make- struct-name)
                                               (symbol-package struct-name))))
             ;;
             (as-alist-sym (sb-int:symbolicate struct-name :-as-alist))
             (*p-func-sym* (option :udef-typep
                                 (sb-int:symbolicate struct-name :-p)))
             ;;
             (nil-value (when (or *index-bits*
                                  wants-nil)
                          (1- (expt 2 (or *index-bits* *max-bits*)))))
             ;;
             (user-slots (mapcar #'make-slot-def
                                   slot-definitions))
             (vector-storage-slot-count (count-if-not #'is-immediate-slot? user-slots))
             ;;
             (var-len-sym (check-var-len-slots user-slots))
             (immediate-bits (reduce #'+
                                     (mapcar (lambda (s)
                                               (if (is-immediate-slot? s)
                                                   (or (slot-byte-length s)
                                                       (error "No length can be determined for slot ~s of ~s. UDEF not yet defined?"
                                                              (cs-slot-slot-name s)
                                                              *current-cs-sym*))
                                                   0))
                                             user-slots)))
             ;;
             (needs-index-slot (or *index-bits*
                                   (plusp vector-storage-slot-count)
                                   (null user-slots)))
             ;; need to fix up the length definitions here
             (%%2 (fixup-bits needs-index-slot immediate-bits))
             ;;
             (*index-slot* (when needs-index-slot
                             (make-slot-def
                               `(,index-name
                                  ,(or nil-value 0)
                                  :type (unsigned-byte ,*index-bits*)
                                  :allocation :immediate))))
             (all-slots (if *index-slot*
                            (cons *index-slot* user-slots)
                            user-slots))
             (accessors (produce-accessors all-slots)))
        (declare (ignore %%1 %%2))
        ;; A sole index slot to identify something is good enough
        (when (zerop (length all-slots))
          (error "Need at least one slot in ~s" struct-name))
        ;; collect basic slot data -- done.
        ;; then run a sanity check,
        (when (> *max-bits* +udef-usable-remaining-bits+)
          (error "*max-bits* too large"))
        (when (< *max-bits* (+ *index-bits* immediate-bits))
          (error "*max-bits* ~d too small, need ~d for index and ~d for immediate slots"
                 *max-bits* *index-bits* immediate-bits))
;; now write code
        `(locally
           (declare (optimize (speed 2) (debug 3) (safety 2)))
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (def-udef-immediate ,struct-name
               :max-bits ,*max-bits*
               :func-sym ,*oper-func*
               ;; NIL gets stored in U-B columns as -1 (mod bits) values
               ;; Note: Number of _index_ bits, not total size including other immediate slots!
               ; TODO configurable?
               :nil-value ,nil-value)
             ;;
             (defvar ,*storage-symbol*
               (vector ,@(loop repeat vector-storage-slot-count
                               collect #())))
             (declaim (type (simple-array simple-array (,vector-storage-slot-count))
                            ,*storage-symbol*)
                      (sb-ext:always-bound ,*storage-symbol*))
           ;;
           (eval-when (:compile-toplevel :load-toplevel :execute)
             ;; TODO: loses value upon reload, keep old contents?
             (sb-vm:without-arena
               (set-c-s-metadata ',struct-name
                                            :batch-size    , *batch-size*
                                            :index-bits    , *index-bits*
                                            :var-len-slot-name  ',var-len-sym
                                            :slots         (coerce ',all-slots 'vector)
                                            ;:var-len-slot  ,(when var-len-sym
                                            ;                  `(find-slot-by-name ,tmp ',var-len-sym))
                                            :has-index-slot ,(and needs-index-slot t)
                                            :data-vec      , *storage-symbol*
                                            :constructor   ', base-constructor
                                            :p-function    ', *p-func-sym*
                                            :as-alist      ', as-alist-sym)
                 ,@(loop for slot in all-slots
                         for init = (slot-argument-init-from-udef slot)
                         for fn = (slot-argument-init-from-udef slot)
                         ;; INIT form must be evaluated
                         collect `(defun ,(cs-slot-init-fn slot) () ,init)))
             (defun ,*p-func-sym* (x)
               (,*oper-func* :typep x))
             ;;
             ;; Only inline for the constructors
             (declaim (inline ,@(mapcar #'second accessors)))
             ,@ accessors
             ,@ (produce-constructors base-constructor constructor-name
                                      all-slots
                                      var-len-sym
                                      *index-slot*)
             ;;
             (defun ,as-alist-sym (obj)
               "Returns OBJ as an ALIST"
               ,(unless user-slots
                  `(declare (ignore obj)))
               (list
                 ;; Don't report the index
                 ,@(loop for slot in user-slots
                         for name = (cs-slot-slot-name slot)
                         for acc = (cs-slot-accessor-sym slot)
                         unless (eq slot *index-slot*)
                         collect `(cons ,(intern (symbol-name name)
                                                 :keyword)
                                        (,acc obj)))))
             ;; Can't keep them inline, breaks resolving the *STORAGE-SYMBOL*
             ;; in FASLs with inline expansion.
             (declaim (notinline ,@(mapcar #'second accessors)))))
           ;; Runtime only
           (column-struct-reset ',struct-name)
           (column-struct-resize ',struct-name ,initial-size)
           ;; done
           ',struct-name)))))


;; ------------------------------------------------------------

(defun set-c-s-metadata (struct-name &rest defs)
  (setf (get struct-name 'column-struct-data)
        (apply #'make-udef-c-s-metadata
               :udef          struct-name
               defs)))

(defun slot-new-element (batch-size mult slot el-type)
  (let* ((init (funcall (cs-slot-init-fn slot))))
    (if batch-size
        (make-array (list (* batch-size
                             (or mult 1)))
                    :element-type el-type
                    :initial-element init)
        init)))

(define-compiler-macro get-new-id-range (&whole whole obj &optional count one-linear-piece?)
  (declare (ignore one-linear-piece?))
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


(defun column-struct-get-struct (obj)
  "Returns the data of OBJ as an alist."
  (assert (udef-immediate-p obj))
  (multiple-value-bind (type) (udef-immediate-type-of obj)
    ;; Optimizations possible
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

(defun increase-element-counts (c-s new-len)
  (with-slots (data-vec slots batch-size) c-s
    (loop for vec across data-vec
          for index upfrom 0
          ;;
          for slot across (remove-if #'is-immediate-slot? slots)
          ;;
          for old-len = (length vec)
          ;; This is too expensive to run in the inner loop below.
          for el-type = (slot-storage-type slot)
          for lower-multiplier = (slot-is-fixed-vector-type slot)
          for this-end = (* new-len
                            (if batch-size
                                1
                                (or lower-multiplier
                                    1)))
          for one-element = (slot-new-element batch-size
                                              lower-multiplier
                                              slot
                                              el-type)
          ;; At first, we write ONE-ELEMENT to all indizes...
          for new-vec = (make-array (list this-end)
                                    :element-type (if batch-size
                                                      `(vector ,el-type)
                                                      el-type)
                                    :initial-element one-element)
          ;; then replace with old contents ...
          do (replace new-vec vec :end1 old-len)
          ;; and finally set new contents,
          ;; keeping ONE-ELEMENT at the OLD-LEN index
          do (loop for i from (1+ old-len) below this-end
                   do (setf (aref new-vec i)
                            (slot-new-element batch-size
                                              lower-multiplier
                                              slot
                                              el-type)))
          do (setf (aref data-vec index)
                   new-vec))))


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
  (declare (optimize (debug 3)))
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
                                    new-count)
             (sb-vm:%write-barrier))))
        ((= new-size (cs-meta-allocated c-s))
         ;; nothing to do after acquiring mutex
         t)
        (t
         ;; enlarge.
         (let* ((2-level? (cs-meta-batch-size c-s))
                (new-count (if 2-level?
                                  (ceiling new-size (cs-meta-batch-size c-s))
                                  new-size)))
           (increase-element-counts c-s new-count)
           (sb-vm:%write-barrier)
           (setf (cs-meta-allocated c-s)
                 (if 2-level?
                     (* new-count (cs-meta-batch-size c-s))
                     new-size))
           (sb-vm:%write-barrier)
           (after-resize-hook (cs-meta-udef c-s)
                              c-s))))))
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
        (when new-initial-size
          (column-struct-resize obj new-initial-size))
        new-initial-size)))

;; ------------------------------------------------------------

(defun check-var-len-slots (slots)
  ;; If there are slots with variable length,
  ;; the structure must contain only :immediate and
  ;; slots with same length specification
  ;; (as the index can only store a single start position).
  ;;
  ;; To allow multiple, different lengths
  ;; we'd need to round up to the largest one
  ;; and accept padding in the other slots.
  (let ((var-len-args ())
        (non-imm-slots ()))
    (dolist (slot slots)
      (multiple-value-bind (dim) (slot-is-vector-type slot)
        (cond
          ((is-immediate-slot? slot)
           ;; ignore
           t)
          (dim
           (when (symbolp dim)
             (pushnew dim var-len-args
                      :test #'eq)))
          (t
           ;; non-immediate slot
           (push (cs-slot-slot-name slot)
                 non-imm-slots)))))
    (case (length var-len-args)
      (0 ;; none, ok
       t)
      (1 ;; only a single length variable used
       ;; TODO: provide a way to combine two structures, using two indizes,
       ;; into one immediate?
       (when non-imm-slots
         (error "Only a single vector index implemented -- ~
                variable-length slots with length ~s and non-immediate slots ~s in the same structure would waste memory."
                var-len-args non-imm-slots)))
      (t
        (error "All slots with variable length vectors must use the same symbol for the length, ~
               and this must be defined as an slot with :ALLOCATION :IMMEDIATE; ~
               found ~s." var-len-args)))
    (first var-len-args)))

;; ------------------------------------------------------------

;; Hook into CL:WITH-SLOTS?
;; sb-mop:slot-value-using-class?
(defmacro with-c-s-slots ((udef-name index) names &body body)
  "Installs symbol-macros that access NAMES from UDEF-NAME
  at INDEX (which can resp. should be a UDEF).
  NAMES can be a list of slot names or a list of (SYM SLOT)."
  (multiple-value-bind (c-s) (get-cs-metadata-from-symbol udef-name t)
    (sb-int:with-unique-names (iidx)
      ; no SB-INT:ONCE-ONLY (index) --
      ;; immediate slots must access the original place!
      ;; TODO: warn on side-effects?
      `(let ((,iidx ,index))
         (if (,(udef-metadata-func udef-name) :typep ,iidx)
           (symbol-macrolet
               ,(loop for entry in names
                      for e-list = (sb-int:ensure-list entry)
                      for var-name = (first e-list)
                      for slot-name = (or (second e-list) var-name)
                      for slot = (or (find-slot-by-name c-s slot-name)
                                     (error "Invalid slot name ~s" slot-name))
                      collect `(,var-name (,(cs-slot-accessor-sym slot) ,index)))
             (progn ,@ body))
             (error "Bad index value ~s, wanted a ~s"
                    ,iidx ',udef-name) )))))


;; ------------------------------------------------------------

(defmacro make-wrapped-udef-accessor (new old col-struct-name)
  "Wraps the (UNSIGNED x) value returned by OLD in NEW
  so that it becomes a UDEF of type COLUMN-STRUCT-TYPE.
  If there's a (SETF OLD), a corresponding wrapper is created as well."
  (let* ((fn-type (sb-introspect:function-type old))
         (ret% (third fn-type))
         (ret-type (if (and (consp ret%)
                            (eq 'values (first ret%)))
                       (second ret%)
                       ret%))
         (old-setf (fboundp `(setf ,old)))
         (udef (get-udef-metadata-from-symbol col-struct-name))
         (op (udef-metadata-func udef)))
    (assert (eq (first ret-type) 'unsigned-byte))
    ;; Must have space for at least as many bits
    (assert (>= (second ret-type) (udef-metadata-max-bits udef)))
    `(progn
       #+(or)
       (declaim (inline ,new)
                ;(ftype (function ,(second fn-type)) (values ,col-struct-name)) ,new)
                )
       ;; TODO: &rest or other args possible? Not on a getter, right?
       (defun ,new (&rest args)
         (,op :int-to-tagged-udef (apply (function ,old) args)))
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
                       (,op :udef-or-nil-to-ub-x ,(first req)))
                 ,(first req)))))
       ',new)))


;; TODO: only map really used entries? WITH-BATCH-MACRO might skip some. need a PK?
(defun map-c-s-range (fn col-struct)
  "Runs MAPCAR over all defined values for COL-STRUCT,
  calling FN with the UDEF."
  (multiple-value-bind (c-s udef) (get-cs-metadata-from-symbol col-struct t)
    (loop for max = (column-struct-last-index c-s)
          for i upfrom 0
          ;; Not a BELOW, because the size may change during iteration
          while (< i max)
          for u = (funcall (udef-metadata-func udef)
                           :int-to-tagged-udef
                           i)
          collect (funcall fn u))))

;; TODO: only map really used entries? WITH-BATCH-MACRO might skip some. need a PK?
(defun c-s-values (col-struct)
  "Returns a list of all defined COL-STRUCT values."
  (multiple-value-bind (c-s udef) (get-cs-metadata-from-symbol col-struct t)
    (loop for i upfrom 0
          ;; Not a BELOW, because the size may change during iteration
          while (< i (column-struct-last-index c-s))
          collect (funcall (udef-metadata-func udef)
                           :int-to-tagged-udef
                           i))))

;; TODO: method dispatch on inner-tagged UDEFs - it's possible on FIXNUM etc. as well!

;; TODO: box/unbox into (unsigned-byte X) specialized arrays and slots
;;       (Without needing a new array widetag for each udef-tag?)

;; TODO: Integration into ROOM, provide statistics about column-struct heap usage

;; TODO: optionally a freelist

;; TODO: integration in rucksack etc.

;; TODO: #\Nul or 0 terminated, eg. for (array character N)

;; ba701174bcab5954d5311047d09fa445d5da9e24 with-slots? Needs classes for DEFMETHOD

;; Pass UDEFs by reference, to make SETF on immediates work?
