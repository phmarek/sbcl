

(require :sb-udef-inttype)


(defstruct my-integers
  (prev nil :type t)
  (v1 0 :type fixnum)
  (v2 0 :type fixnum))

(defparameter *data* (loop for i from 0
                           for prev previous instance
                           for instance = (make-my-integers :prev prev
                                                            :v1 i
                                                            :v2 (mod i 37))
                           finally (return instance))

(save-lisp-and-die "/tmp/slad-comparison" :executable t)

