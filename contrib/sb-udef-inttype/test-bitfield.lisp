(defpackage #:udef-bitfield-tests
  (:use #:cl))

(in-package #:udef-bitfield-tests)

(require :sb-udef-inttype)

#+(or)
(defmacro deftest (name form &rest vals)
  `(let ((got (multiple-value-list ,form))
         (want (list ,@ vals)))
     (assert (equal got want)
             ()
             "Test ~s broken: got ~s, wanted ~s" ',name got want)))


(sb-udef-inttype:def-column-struct (bbbits
                                     (:index-bits 0))
  (eight 255 :type (unsigned-byte 8) :modulo t)
  (four . 4)
  (one 1 :type (unsigned-byte 1)))


(defparameter *default* (make-bbbits))
(defparameter *own* (make-bbbits :eight #x55 :four #xa :one 0))

(deftest udef-bitfield.d0 (bbbits-eight *default*) 255)
(deftest udef-bitfield.d1 (bbbits-four *default*) 0)
(deftest udef-bitfield.d2 (bbbits-one *default*) 1)

(deftest udef-bitfield.o0 (bbbits-eight *own*) #x55)
(deftest udef-bitfield.o1 (bbbits-four *own*) 10)
(deftest udef-bitfield.o2 (bbbits-one *own*) 0)

(deftest udef-bitfield.error.0
  (handler-case
      (make-bbbits :four -5)
    (simple-type-error (c)
      (declare (ignore c))
      t)
    (:no-error (&rest x) x))
  t)

(deftest udef-bitfield.error.1
  (handler-case
      (make-bbbits :four 256)
    (simple-type-error (c)
      (declare (ignore c))
      t)
    (:no-error (&rest x) x))
  t)

(deftest udef-bitfield.limit.0
  (and (make-bbbits :eight 255)
       t)
  t)

(deftest udef-bitfield.incf-no-overflow
  (let* ((x (make-bbbits :four #x0e :eight #xf0)))
    (incf (bbbits-eight x)
          (incf (bbbits-four x)))
    (values (bbbits-eight x)
            (bbbits-four x)))
  #xff
  #x0f)

(deftest udef-bitfield.setf-car
  (let* ((x (make-bbbits))
         (cell (cons x nil)))
    (setf (bbbits-eight (car cell))
          3)
      (bbbits-eight (car cell)))
  3)

(deftest udef-bitfield.incf
  (let* ((x (make-bbbits :four #x02 :eight #x20)))
    (incf (bbbits-eight x) #x4)
    (incf (bbbits-four x) #x4)
    (values (bbbits-eight x)
            (bbbits-four x)))
  #x24
  #x06)

