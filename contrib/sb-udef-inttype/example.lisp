(defpackage :udef-inttype-example
  (:use :cl :sb-int)
  (:import-from :sb-impl #:def-udef-inttype)
  )
(in-package :udef-inttype-example)


(def-udef-inttype my-idx-51
  :constructor make-my-idx
   :max-bits 15)

(def-udef-inttype my-2bit
  :constructor make-my-2bit
   :max-bits 2)



(defmethod print-object ((obj my-idx-51) s)
  (print-unreadable-object (obj s :type t :identity t)
    (format s "0x~x" (get-my-idx-51-value obj))))

;; doesn't trigger,
;; how to match arguments?
(sb-mop:compute-applicable-methods
  #'print-object (list (make-my-idx #x1222) *standard-output*))


(sb-impl::udef-inttype-type-of
  (make-my-idx #xfe))

sb-impl::*udef-types*
(sb-int:udef-inttype-p
  (make-my-idx #xfe))

(sb-int:udef-inttype-p
  (make-my-idx #xfe))

(type-of
  (make-my-idx #xfe))

#+(or)
(make-instance 'my-idx-51)

(disassemble #'make-my-idx)

;; extend src/code/class.lisp to get subtypes recognized?
#+(or)
(my-idx-51-p
  (make-my-idx #xfe))

(my-idx-51-p (make-my-bit 1))
(my-2bit-p (make-my-2bit 1))
(my-2bit-p (make-my-2bit 0))
(my-2bit-p (make-my-2bit 2))
(my-idx-51-p (make-my-2bit 3))

(format nil "~x"
  (sb-kernel:get-lisp-obj-address (make-my-idx #xfe)))

(defun to-be-traced (x)
    (type-of x))

(to-be-traced (make-my-idx #x151))
(to-be-traced (make-my-2bit #x1))
(sb-c::layout-of (make-my-idx #x151))
(sb-c::%instancep (make-my-idx #x151))


(defparameter *vec* (make-array 5 :element-type '(unsigned-byte 16)
                                :initial-element 0))

(defparameter *my-v* (make-my-idx x15))

(setf (aref *vec* 3) *vec*)

*vec*

  :with-batch-macro with-my-idx

#|
(defclass udef-meta-class ()
  (
   #+(or)(sb-mop:class-default-initargs :initform ())
   )
  )


(defmethod make-instance ((class udef-meta-class) &key)
  nil)

(defclass my-enum-1 (sb-int:udef-inttype)
  ((id :initform (error "need ID")
       :initargs id
       :type (unsigned-byte 8)))
  (:metaclass udef-meta-class))


(progn
  (eval `(defmethod make-instance ((class symbol) &key)
           nil))
  (trace "SB-PCL")
  (eval `(defmethod make-instance ((class character) &key)
           nil))
  (untrace "SB-PCL"))



(defvar *pointers* (make-array 200
                               :element-type ()))


(defmacro def-udef-inttype (name (&keys id))
  (let ((make-fn (intern (format nil "~a-~a" :MAKE name)
                     (symbol-package name))))
  `(progn
     (deftype ,name ())
     (declaim (inline ,make-fn)
              (ftype (function ((unsigned-byte 48)) ,name)))
     (defun ,make-fn (i)
       (sb-int:make-udef-inttype (logior ,id (ash i 8)))))))
|#

