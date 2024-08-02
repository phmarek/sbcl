(defpackage :new-uint-type
  (:use :cl))

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
