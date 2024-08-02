;;;; User-defined integer types / enumerations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")
;(in-package "SB-UDEF-INTTYPE")


;;; High-level interface -- multiple sub-types
;;
;; Apart from the bits used by udef-inttype-lowtag,
;; this here declares a further
(defconstant +udef-tag-bits+ 8)
;; bits to be used for a range of user-defined integer types.


(defconstant +udef-usable-remaining-bits+ (- sb-vm:n-word-bits
                                             +udef-tag-bits+
                                             sb-vm:n-widetag-bits))

(defparameter *udef-types* nil
  "Vector of user-defined integer types.
  NIL until first use.")


(defun register-udef-subtype-id (type-name &key (random t))
  "Looks for a free ID in *UDEF-TYPES*,
  and registers TYPE-NAME.
  Not thread-safe."
  (check-type type-name symbol)
  (let ((len (expt 2 +udef-tag-bits+)))
    (unless *udef-types*
      (setf *udef-types* (make-array len
                                     :initial-element nil
                                     :element-type 'symbol)))
    (let ((id (or (position type-name *udef-types* :test #'eq)
                  (when random
                    (loop repeat 3
                          for i = (random len)
                          if (null (aref *udef-types* i))
                          return i
                          finally (return
                                    (position nil *udef-types*
                                              :start (random len)))))
                  (position nil *udef-types*))))
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

#+(or)
(declaim (ftype (function (udef-inttype) (unsigned-byte #.+udef-tag-bits+))
                udef-inttype-tag))
(defun udef-inttype-tag (x)
  (logand (1- (ash 1 +udef-tag-bits+))
          (udef-inttype-value x)))

(declaim (inline udef-general-get-value))
(defun udef-general-get-value (x)
  (ash (udef-inttype-value x)
       (- +udef-tag-bits+)))

(declaim (ftype (function (T)
                          (values symbol
                                  (unsigned-byte #.(- sb-vm:n-word-bits sb-vm:n-widetag-bits))))
                          udef-inttype-type-of))
(defun udef-inttype-type-of (x)
  "Returns the type symbol, and the content as second value."
  ;; TODO: error out if not a udef-inttype?
  (if (udef-inttype-p x)
    (let ((type (aref *udef-types* (udef-inttype-tag x))))
      (if type
          (values type
                  ;; Possibly we should call using the class' reader slot --
                  ;; but that indirection is much slower than just shifting the bits.
                  ;; (funcall (sb-mop:slot-value-using-class type 'udef-reader) )
                  (udef-general-get-value x))
          (values 'udef-inttype
                  (sb-int:udef-inttype-value x))))
      ;; TODO: (error "not a udef-inttype") ??
      (values nil 0)))


(defmacro def-udef-inttype (name &key constructor
                                 reader
                                 id
                                 typep
                                 (max-bits +udef-usable-remaining-bits+))
  "Defines a new user-defined integer type."
  (let* ((old-id (position name *udef-types*))
         (id (cond
               ((and old-id       id  (= old-id id))  id)
               ((and (not old-id) id)                 id)
               (old-id                            old-id)
               (t
                (register-udef-subtype-id name))))
         (typep-fn (or typep
                       (intern (format nil "~a-~a" name :p)
                               (symbol-package name))))
         (read-fn (or reader
                      (intern (format nil "~a-~a-~a" :get name :value)
                              (symbol-package name))))
         (make-fn (or constructor
                      (intern (format nil "~a-~a" :MAKE name)
                              (symbol-package name)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (deftype ,name () 'sb-int:udef-inttype)
         (defclass ,(gensym (format nil "UDEF-~a" name)) ()
           ((udef-id :initform ,id
                     :allocation :class)
            (udef-max-bits :initform ,max-bits
                      :allocation :class)
            (udef-maker :initform ',make-fn
                   :allocation :class)
            (udef-reader :initform ',read-fn
                    :allocation :class)
            (udef-value :initform (error "~s cannot be allocated via MAKE-INSTANCE, please use ~s"
                                    ',name ',make-fn)
                   :initarg :value))
           #+(or)
           (:metaclass sb-kernel:ctype)))
       ;; TODO: box/unbox
       (declaim (inline ,typep-fn)
                (ftype (function (T) (member t nil)) ,typep-fn))
       (declaim (inline ,read-fn)
                (ftype (function (,name) (unsigned-byte ,max-bits)) ,read-fn))
       (declaim (inline ,make-fn)
                (ftype (function ((unsigned-byte ,max-bits)) T #+(or),name) ,make-fn))
       (defun ,typep-fn (x)
         (= ,id (udef-inttype-tag x)))
       (defun ,read-fn (x)
         (udef-general-get-value x))
       (defun ,make-fn (x)
         (make-udef-inttype (logior ,id
                                    (ash x +udef-tag-bits+))))
       ',name)))

;; TODO: not exported after building??
;; TODO: subclasses for MOP methods
