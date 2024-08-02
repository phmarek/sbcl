(assert (not (typep 4 'sb-kernel::udef-inttype)))
(assert (not (sb-int:udef-inttype-p 4)))

(sb-int:udef-inttype-p
  (sb-int:make-udef-inttype 51))

#+broken
(assert
  (sb-int:udef-inttype-p
    (sb-int:make-udef-inttype 51)))



(defvar *x* (make-array 20000 :initial-element (sb-int:make-udef-inttype 541)))
(sb-ext:gc)

(print (sb-int:udef-inttype-value (aref *x* 0)))
(print (type-of (aref *x* 0)))
(print (aref *x* 0))
(print (sb-int:udef-inttype-p (aref *x* 0)))


(defmethod my-dispatch ((y t))
  :anything)
(defmethod my-dispatch ((x sb-int:udef-inttype))
    :udef)

#+broken
(my-dispatch (sb-int:make-udef-inttype 1))

(assert (eq :anything
            (my-dispatch 1)))

#+broken
(assert (eq :udef
            (my-dispatch (sb-int:make-udef-inttype 1))))


