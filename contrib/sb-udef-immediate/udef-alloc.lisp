;;;; User-defined integer types

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package :sb-udef-immediate)


;;; High-level interface -- multiple sub-types
;;

(defconstant +udef-reserved-low-bits+ (+ sb-vm:n-widetag-bits
                                         +udef-tag-bits+))
(defconstant +udef-usable-remaining-bits+ (- sb-vm:n-word-bits
                                             +udef-reserved-low-bits+))


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
      (declare (type (unsigned-byte 8) id))
      (unless id
        (error "too many user-defined integer types in use."))
      (setf (aref *udef-types* id)
            type-name)
      id)))

;; with superclass ctype, (:metaclass sb-kernel::structure-class))
;; #<SIMPLE-ERROR "~S instance constructor called in a non-system file" {1002966753}>
;
;(defclass udef-immediate () ())
;; SB-KERNEL:CTYPE instance constructor called in a non-system file
;;    [Condition of type SIMPLE-ERROR]

(declaim (inline udef-general-get-value))
(defun udef-general-get-value (x)
  (ash (sb-kernel:get-lisp-obj-address x)
       (- (+ sb-vm:n-widetag-bits +udef-tag-bits+))))

(declaim (inline make-twice-tagged-udef)
         (ftype (function ((unsigned-byte #. +udef-tag-bits+)
                           (unsigned-byte #. +udef-usable-remaining-bits+))
                          (values t ;udef-immediate
                                  ))
                make-twice-tagged-udef))

(defun make-twice-tagged-udef (tag value)
  (sb-kernel:%make-lisp-obj
    (logior (ash value +udef-reserved-low-bits+)
            (logior (ash tag sb-vm:n-widetag-bits)
                    udef-immediate-widetag))))

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
                       udef-immediate-widetag))
         (have (ldb (byte +udef-reserved-low-bits+ 0)
                    v)))
    (when (= want have)
      (ldb (byte +udef-usable-remaining-bits+ +udef-reserved-low-bits+)
           v))))

(declaim (inline check-tagged-udef-value)
         (ftype (function ((unsigned-byte #. +udef-tag-bits+)
                           t ; udef-immediate
                           )
                          (values (unsigned-byte #. +udef-usable-remaining-bits+)))
                check-tagged-udef-value))

(defun check-tagged-udef-value (tag udef)
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (or (is-tagged-udef-value tag udef)
      (error "need a udef with tag #x~x (~a), not a ~s"
             tag
             (and *udef-types* (aref *udef-types* tag))
             udef)))

(defstruct udef-metadata
  (udef-id        0  :type (integer 0 255)   :read-only t)
  (udef-sym     nil  :type symbol            :read-only t)
  ;; A slot called TYPEP doesn't work with default args in DEFSTRUCT
  (func%        nil  :type symbol            :read-only t)
  (nil?         nil  :type (or null fixnum)  :read-only t)
  (max-bits       0  :type (or null (integer 1 48)) :read-only t))

(export '( udef-metadata
           udef-metadata-udef-id
           udef-metadata-func
           udef-metadata-udef-sym
           udef-metadata-max-bits
           make-twice-tagged-udef
           check-tagged-udef-value))

(defun udef-metadata-func (v)
  (cond
    ((null v)
     nil)
    ((typep v 'udef-metadata)
     (udef-metadata-func% v))
    ((typep v 'symbol)
     (udef-metadata-func
       (get v 'udef-metadata)))
    (t
     (error "Bad type: ~s" v))))

(defun setup-udef (name &rest args)
  (setf (get name 'udef-metadata)
        (apply #'make-udef-metadata args)))

(defmacro def-udef-immediate (name &key id func-sym
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
    (declare (type (unsigned-byte 8) id))
    ;; (DEBUG 1) is necessary to keep the argument and result types
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,name (:include udef-immediate)
                           (:constructor nil)
                           (:predicate ,pred)
                           (:copier nil)
                           ))
         ;;
         (declaim (inline ,pred))
         (setf (symbol-function ',pred)
               (lambda (input)
                 (if (is-tagged-udef-value ,id input)
                     t
                     nil)))
         ;;
         (let ((got (register-udef-subtype-id ',name :preferred ,id)))
           (assert (eql ,id got))
         (setup-udef ',name
                     :udef-id  ,id
                     :nil?     ,nil?
                     :func%    ',func-sym
                     :udef-sym ',name
                     :max-bits ,max-bits))
         ;;
         ;(declaim (inline ,func-sym))
         (locally
           (declare (sb-ext:muffle-conditions warning))
           (defun ,func-sym (operation input)
             (ecase operation
               (:typep
                (when (is-tagged-udef-value ,id input)
                  t))
               (:int-to-tagged-udef
                (check-type input (integer 0 ,mask))
                (make-twice-tagged-udef ,id input))
               (:tagged-udef-to-int
                (the (integer 0 ,mask)
                     (check-tagged-udef-value ,id input)))
               (:udef-or-nil-to-ub-x
                (if (null input)
                    (if ,nil?
                        ,nil?
                        (error "NIL not allowed for udef ~s" ',name))
                    (the (integer 0 ,mask)
                         (check-tagged-udef-value ,id input))))
               (:ub-x-to-udef-or-nil
                (if (and ,nil?
                         (eql input ,nil?))
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


(defmethod print-object ((obj udef-immediate) stream)
  (let* ((v (sb-kernel:get-lisp-obj-address obj))
         (utag (ldb (byte +udef-tag-bits+ sb-vm:n-widetag-bits) v)))
    (format stream "#<UDEF #x~x~@[; tagged #x~x~]~@[, a ~s~]>"
            (ash v (- +udef-reserved-low-bits+))
            utag
            (and *udef-types*
                 (aref *udef-types* utag)))))
