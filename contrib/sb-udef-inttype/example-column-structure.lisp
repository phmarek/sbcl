(defpackage :col-struct-example
  (:use :cl)
  )
(in-package :col-struct-example)


(require :sb-udef-inttype)

#+(or)
(setf sb-impl::*workaround-type-def* 2)

(sb-udef-inttype:def-column-struct (foo
                                     (:index-bits 17)
                                     (:initial-size 10)
                                     (:udef-typep foo-p))
  (a "" :type string)
  (b 0 :type (unsigned-byte 16))
  (c 22 :type fixnum))

(defparameter *no-foo* (make-foo :a ":a"))


;; Test pre-declaration, for self-referencing stuff
(sb-udef-inttype::def-udef-inttype bar
  :nil-value t
  :max-bits 22)

(sb-udef-inttype:def-column-struct (bar
                                      (:index-bits 22)
                                      (:batched 8)
                                      (:constructor make-my-bar)
                                      (:base-constructor make-my-bar-base)
                                      (:initial-size 50)
                                      (:batch-size 1000)
                                      (:with-batch-allocation-name with-bar-batch))
  (name :name :type symbol)
  (i 2 :type (unsigned-byte 32))
  (vec 3 :type (array (unsigned-byte 32) (3)))
  (typed-ref% 4 :type (unsigned-byte 32))
  (ref *no-foo* :type T)
  (self nil :type bar)
  (self-vec nil :type (array bar (4))) ;; TODO
  (udef *no-foo* :type foo))


(defparameter *a-bar* (make-my-bar :name 'first
                                   :i 5515
                                   :ref nil
                                   :vec (make-array 3 :element-type '(unsigned-byte 32)
                                                    :initial-contents '(1 2 3))))

;;; Test code

(sb-udef-inttype:def-column-struct (contains-lists
                                     (:max-bits 24)
                                     (:index-bits 8)
                                     (:initial-size 10))
  (l  nil :type list))


(sb-udef-inttype:def-column-struct (var-len-string
                                     (:index-bits 8)
                                     (:initial-size 16))
  (l     0 :type (unsigned-byte 8) :allocation :immediate)
  (vec #\. :type (array character (l))))

(sb-udef-inttype:def-column-struct (udef-cons
                                     (:index-bits 8)
                                     (:max-bits 24)
                                     (:initial-size 16))
  (car nil :type udef-cons :allocation :immediate)
  (cdr nil :type udef-cons :allocation :immediate))

(defvar *leaf-1* (make-udef-cons :car nil :cdr nil))


(sb-udef-inttype:def-column-struct (no-slot-udef
                                     (:max-bits 2)))

;; An extended CONS ;)
(sb-udef-inttype:def-column-struct (cocoon
                                     (:index-bits 4)
                                     (:max-bits 16))
  (car  nil    :type cocoon :allocation :immediate)
  (cdr  nil    :type cocoon :allocation :immediate)
  (cgr  nil    :type cocoon :allocation :immediate))


(sb-udef-inttype:def-column-struct (bcd
                                     (:max-bits 48))
  (digit1  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit2  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit3  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit4  0  :type (unsigned-byte 4)  :allocation :immediate))

(sb-udef-inttype:def-column-struct (batched-var-len-string
                                     (:max-bits 16)
                                     (:batch-size 10)
                                     (:initial-size 16)
                                     (:with-batch-allocation-name with-bvls))
  (l     0 :type (unsigned-byte 8) :allocation :immediate)
  (vec #\. :type (array character (l))))

(sb-udef-inttype:def-column-struct (ref-2-other-udefs
                                     (:batch-size (* 256 1024))
                                     (:index-bits 32))
  (len   0   :type (unsigned-byte 16) :allocation :immediate)
  (foos nil :type (array foo (len)))
  (bars  nil :type (array bar (len))))

