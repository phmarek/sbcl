

(require :sb-udef-inttype)

#. (let ((what (second sb-ext:*posix-argv*)))
     (defparameter *def* (if (string= what "1")
                             'defstruct
                             'sb-column-struct:def-column-struct))
     (defparameter *name* (cond
                            ((string= what "3")
                             ;; A size that, sized up a few times, reaches 10M.
                              '(my-integers (:initial-size 3568000)))
                            ((string= what "4")
                              '(my-integers (:initial-size 3000000)
                                            (:batched 1000000)))
                            (t ; 1 & 2
                              'my-integers))))

(#. *def*  #. *name*
  (prev nil :type t)
  (v1 0 :type fixnum)
  (v2 0 :type fixnum))

(defparameter *data* (loop for i from 0 to 10000000
                           for instance = (make-my-integers :prev instance
                                                            :v1 i
                                                            :v2 (mod i 37))
                           finally (return instance)))

(unless (eq *def* 'defstruct)
  (format t "Final size: ~d~%"
          (sb-column-struct:column-struct-size 'my-integers)))

(save-lisp-and-die (third sb-ext:*posix-argv*) :executable t)

