(in-package :cl-user)

(defmethod do-that ((obj foo))
  (format t "Yeah, got ~a!~%" obj))

(defparameter *obj* (make-foo :slot1 42))
