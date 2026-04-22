(require :sb-udef-immediate)

(defpackage :col-struct-example
  (:use :cl :sb-udef-immediate)
  )
(in-package :col-struct-example)




(sb-udef-immediate:def-column-struct (foo
                                     (:index-bits 17)
                                     (:initial-size 10)
                                     (:base-constructor make-foo%)
                                     (:udef-typep foo-p))
  (a "" :type string)
  (b 0 :type (unsigned-byte 16))
  (c 22 :type fixnum))

(defparameter *no-foo* (make-foo :a ":a"))

(defmethod print-object ((obj foo) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(sb-udef-immediate:def-column-struct (bar
                                      (:index-bits 22)
                                      (:constructor make-my-bar)
                                      (:base-constructor make-my-bar-base)
                                      (:initial-size 50)
                                      (:batch-size 1000))
  (name          :name :type symbol)
  (i                 2 :type (unsigned-byte 32))
  (vec               3 :type (array (unsigned-byte 32) (3)))
  (typed-ref%        4 :type (unsigned-byte 32))
  (ref        *no-foo* :type T)
  (self            nil :type bar)
  (self-vec        nil :type (array bar (4))) ;; TODO
  (udef       *no-foo* :type foo))

; (upgraded-array-element-type bar)

(defparameter *a-bar* (make-my-bar :name 'first
                                   :i 5515
                                   :ref nil
                                   :vec (make-array 3 :element-type '(unsigned-byte 32)
                                                    :initial-contents '(1 2 3))))

;;; Test code

(sb-udef-immediate:def-column-struct (contains-lists
                                     (:max-bits 24)
                                     (:index-bits 8)
                                     (:initial-size 10))
  (l  nil :type list))


(sb-udef-immediate:def-column-struct (var-len-string
                                     (:index-bits 8)
                                     (:initial-size 16))
  (l     0 :type (unsigned-byte 8) :allocation :immediate)
  (vec #\. :type (array character (l))))

(sb-udef-immediate:def-column-struct (udef-cons
                                     (:index-bits 8)
                                     (:max-bits 24)
                                     (:initial-size 16))
  (car nil :type udef-cons :allocation :immediate)
  (cdr nil :type udef-cons :allocation :immediate))

(defvar *leaf-1* (make-udef-cons :car nil :cdr nil))


(sb-udef-immediate:def-column-struct (no-slot-udef
                                     (:max-bits 4)
                                     (:reserve-nil-value t)))

;; An extended CONS ;)
;; cannot address itself in the slots - the data aren't stored anywhere
;; but in the UDEF, but then it becomes to big to be stored within itself
(sb-udef-immediate:def-column-struct (cocoon
                                     (:index-bits 4)
                                     (:max-bits 16))
  (car  nil    :type no-slot-udef :allocation :immediate)
  (cdr  nil    :type no-slot-udef :allocation :immediate)
  (cgr  nil    :type no-slot-udef :allocation :immediate))


(sb-udef-immediate:def-column-struct (bcd
                                     (:max-bits 48))
  (digit1  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit2  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit3  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit4  0  :type (unsigned-byte 4)  :allocation :immediate))

(sb-udef-immediate:def-column-struct (batched-var-len-string
                                     (:max-bits 16)
                                     (:batch-size 10)
                                     (:initial-size 16))
  (l     0 :type (unsigned-byte 8) :allocation :immediate)
  (vec #\. :type (array character (l))))

(sb-udef-immediate:def-column-struct (ref-2-other-udefs
                                     (:batch-size (* 256 1024))
                                     (:index-bits 32))
  (len   0   :type (unsigned-byte 16) :allocation :immediate)
  (foos nil :type (array foo (len)))
  (bars  nil :type (array bar (len))))

