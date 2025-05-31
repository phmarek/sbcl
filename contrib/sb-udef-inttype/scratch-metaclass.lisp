(require :sb-udef-inttype)
(in-package :sb-udef-inttype)


(def-udef-inttype blubber :max-bits 22)

(ql:quickload :clouseau)
;
;(defclass col-struct-class (standard-object)
;  ((name)))
;
;
;#+(or)
;(defclass col-struct-struct (structure-object)
;  ((name)))
;
;(defclass foo1 (col-struct-class)
;  (aa))
;

(sb-thread:make-thread (lambda ()
  (clouseau:inspect 'sb-kernel:built-in-classoid)))



(defclass ffofo2 (sb-kernel:built-in-classoid)
  (;(sb-kernel::%bits 312)
   (name)))

sb-c::ctype

sb-pcl::type-class

sb-kernel::ctype-contains-class

sb-kernel::*type-classes*
sb-kernel::*type-class-list*

sb-c::intersection-type


(sb-kernel::make-type-class :name 'fofof11
                            )

'sb-c::alien-type-type

(defun make-blubber2 ()
  (udef/blubber-operation :int-to-tagged-udef 0))

(sb-kernel::def-type-model (blubber (:constructor* make-blubber2 ()))
  )

