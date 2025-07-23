;;;; User-defined integer types / enumerations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-UDEF-INTTYPE")


;; set the PREDICATE slot
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (slot-value (sb-c::find-classoid 'udef-inttype)
                    'sb-kernel::predicate)
        (symbol-function 'sb-int::udef-inttype-p)))

;;; High-level interface -- multiple sub-types
;;


(defconstant +udef-reserved-low-bits+ (+ sb-vm:n-widetag-bits
                                         +udef-tag-bits+))
(defconstant +udef-usable-remaining-bits+ (- sb-vm:n-word-bits
                                             +udef-reserved-low-bits+))


(defvar *udef-types* nil
  "Vector of user-defined integer types.
  NIL until first use.")


(defun get-existing-udef-id (name &key (start 0))
  (when *udef-types*
    (position name *udef-types*
              :test #'eq
              :start start)))

(defun register-udef-subtype-id (type-name &key (random t) preferred)
  "Looks for a free ID in *UDEF-TYPES*,
  and registers TYPE-NAME.
  Not thread-safe."
  (check-type type-name symbol)
  (let ((len (expt 2 +udef-tag-bits+)))
    (unless *udef-types*
      (setf *udef-types* (make-array len
                                     :initial-element nil
                                     :element-type 'symbol)))
    (let ((id (or (get-existing-udef-id type-name)
                  preferred
                  (when random
                    (loop repeat 3
                          for i = (random len)
                          if (null (aref *udef-types* i))
                          return i
                          finally (return
                                    (get-existing-udef-id nil
                                                          :start (random len)))))
                  (get-existing-udef-id nil))))
      (unless id
        (error "too many user-defined integer types in use."))
      (setf (aref *udef-types* id)
            type-name)
      id)))

;; with superclass ctype, (:metaclass sb-kernel::structure-class))
;; #<SIMPLE-ERROR "~S instance constructor called in a non-system file" {1002966753}>
;
;(defclass udef-inttype () ())
;; SB-KERNEL:CTYPE instance constructor called in a non-system file
;;    [Condition of type SIMPLE-ERROR]

(declaim (inline udef-general-get-value))
(defun udef-general-get-value (x)
  (ash (sb-kernel:get-lisp-obj-address x)
       (- (+ sb-vm:n-widetag-bits +udef-tag-bits+))))

(declaim (inline make-twice-tagged-udef)
         (ftype (function ((unsigned-byte #. +udef-tag-bits+)
                           (unsigned-byte #. +udef-usable-remaining-bits+))
                          (values udef-inttype))
                make-twice-tagged-udef))
(defun make-twice-tagged-udef (tag value)
  (sb-kernel:%make-lisp-obj
    (logior (ash value +udef-reserved-low-bits+)
            (logior (ash tag sb-vm:n-widetag-bits)
                    udef-inttype-widetag))))

(declaim (inline is-tagged-udef-value)
         (ftype (function ((unsigned-byte #. +udef-tag-bits+) T)
                          (values (or (unsigned-byte #. +udef-usable-remaining-bits+)
                                      null)))
                is-tagged-udef-value))

(defun is-tagged-udef-value (tag udef)
  "If UDEF is a UDEF and tagged as TAG, return its value."
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (let* ((v (sb-kernel:get-lisp-obj-address udef))
         (want (logior (ash tag sb-vm:n-widetag-bits)
                       udef-inttype-widetag))
         (have (ldb (byte +udef-reserved-low-bits+ 0)
                    v)))
    (when (= want have)
      (ldb (byte +udef-usable-remaining-bits+ +udef-reserved-low-bits+)
           v))))

(declaim (inline check-tagged-udef-value)
         (ftype (function ((unsigned-byte #. +udef-tag-bits+)
                           udef-inttype)
                          (values (unsigned-byte #. +udef-usable-remaining-bits+)))
                check-tagged-udef-value))

(defun check-tagged-udef-value (tag udef)
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (or (is-tagged-udef-value tag udef)
      (error "need a udef with tag #x~x, not a ~s" tag udef)))

(defstruct udef-metadata
  (udef-id        0  :type (integer 0 255)   :read-only t)
  ;; A slot called TYPEP doesn't work with default args in DEFSTRUCT
  (func         nil  :type symbol            :read-only t)
  (nil?         nil  :type (or null fixnum)  :read-only t)
  (max-bits       0  :type (integer 1 48)    :read-only t))

(export '( udef-metadata
           udef-metadata-udef-id
           udef-metadata-func
           udef-metadata-udef-sym
           udef-metadata-max-bits
           make-twice-tagged-udef
           check-tagged-udef-value))

(defmacro def-udef-inttype (name &key id func-sym
                                 (nil-value t)
                                 (max-bits +udef-usable-remaining-bits+))
  "Defines a new user-defined integer type."
  (let* ((old-id (get-existing-udef-id name))
         (id (cond
               ((and old-id       id  (= old-id id))  id)
               ((and (not old-id) id)                 id)
               (old-id                            old-id)
               (t
                (register-udef-subtype-id name))))
         (pred (sb-int:gensymify* name :-p))
         (mask (1- (ash 1 max-bits)))
         (nil? (if (eq t nil-value)
                   mask
                   nil-value))
         ;; TODO: use *PACKAGE* instead of the NAMEs package?
         (func-sym (or func-sym
                       (sb-int:symbolicate :UDEF/ name :-OPERATION))))
    ;; (DEBUG 1) is necessary to keep the argument and result types
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (deftype ,name () 'sb-int:udef-inttype)
         ;; TODO unseal/seal CTYPE, CLASSOID, BUILT-IN-CLASSOID?
         #+(or)
         (defstruct (,name (;:include sb-c::built-in-classoid
                            ;sb-int:udef-inttype
                            )
                           (:constructor nil)
                           (:predicate ,pred)
                           (:copier nil)))
         (sb-c:defknown ,pred (t) (values t)
             sb-c::(any dx-safe foldable flushable))
         (sb-c::define-type-predicate ,pred ,name)
         ;;
         (assert (= ,id
                    (register-udef-subtype-id ',name :preferred ,id)))
         (unless (get ',name 'udef-metadata)
           (setf (get ',name 'udef-metadata)
                 (make-udef-metadata
                   :udef-id  ,id
                   :nil?     ,nil?
                   :func     ',func-sym
                   :max-bits ,max-bits))
           ;;
           (declaim (inline ,pred))
           (defun ,pred (input)
             (if (is-tagged-udef-value ,id input)
                 t
                 nil))
           ;;
           (declaim (inline ,func-sym))
           (defun ,func-sym (operation input)
             (ecase operation
               (:typep
                (when (is-tagged-udef-value ,id input)
                  t))
               (:int-to-tagged-udef
                (check-type input (integer 0 ,mask))
                (make-twice-tagged-udef ,id input))
               (:tagged-udef-to-int
                (check-tagged-udef-value ,id input))
               (:udef-or-nil-to-ub-x
                (if (null input)
                    (if ,nil?
                        ,nil?
                        (error "NIL not allowed for udef ~s" ',name))
                    (check-tagged-udef-value ,id input)))
               (:ub-x-to-udef-or-nil
                (if (and ,nil?
                         (= input ,nil?))
                    nil
                    (make-twice-tagged-udef ,id input)))
               (:mask
                ,mask)
               (:nil?
                ,nil?)
               (:udef-id
                ,id)))))
       ;;
       (values ',name
               ,id))))


(defun udef-eq (u1 u2)
  (= (sb-kernel:get-lisp-obj-address u1)
     (sb-kernel:get-lisp-obj-address u2)))

(defun hash-udef (u)
  ;; Do we need equivalence if a type FOO gets removed and
  ;; re-defined with another udef tag?
  ;; TODO: use SXHASH or PSXHASH?
  (sb-kernel:get-lisp-obj-address u))

(sb-ext:define-hash-table-test udef-eq hash-udef)


(defmethod print-object ((obj sb-int:udef-inttype) stream)
  (let* ((v (sb-kernel:get-lisp-obj-address obj))
         (utag (ldb (byte +udef-tag-bits+ sb-vm:n-widetag-bits) v)))
    (format stream "#<UDEF #x~x~@[; tagged #x~x~]~@[, a ~s~]>"
            (ash v (- +udef-reserved-low-bits+))
            utag
            (and *udef-types*
                 (aref *udef-types* utag)))))

;; TODO: (udef-tag-p udef tag) with compiler-macro

;; TODO: subclasses for MOP methods
