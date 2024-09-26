

(require :sb-udef-inttype)

#. (let ((what (second sb-ext:*posix-argv*)))
     (defparameter *def* (if (string= what "1")
                             'defstruct
                             'sb-column-struct:def-column-struct))
     (defparameter *name* (if (string= what "3")
                              '(my-integers (:initial-size 6000000))
                              'my-integers)))

(#. *def*  #. *name*
  (prev nil :type t)
  (v1 0 :type fixnum)
  (v2 0 :type fixnum))

(defparameter *data* (loop for i from 0 to 5000000
                           for instance = (make-my-integers :prev instance
                                                            :v1 i
                                                            :v2 (mod i 37))
                           finally (return instance)))

(save-lisp-and-die "/tmp/slad-comparison" :executable t)

