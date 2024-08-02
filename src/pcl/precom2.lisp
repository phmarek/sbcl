;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(macrolet ((precompile-random-code-segments (&optional system)
             `(progn
                (eval-when (:compile-toplevel)
                  (update-dispatch-dfuns))
                (precompile-function-generators ,system)
                (precompile-dfun-constructors ,system)
                (precompile-ctors))))
  (precompile-random-code-segments pcl))

#+(or)
(push '("SB-PCL" *built-in-classes*) *!removable-symbols*)

(defun !system-class-p (x) (typep x 'sb-pcl::system-class))

(let ((c (find-class 't)))
  (assert (not (slot-boundp c 'sb-pcl::prototype))))
(let ((class (find-class 'sequence)))
  ;; Give the prototype a concrete prototype. It's an extra step because
  ;; SEQUENCE was removed from *built-in-classes*
  (setf (slot-value class 'prototype) #()))
#+(or)
(let ((class (find-class 'udef-inttype)))
  (setf (slot-value class 'prototype)
        ;(%make-lisp-obj udef-inttype-lowtag)
        ; pre-GC failure
        ; Ptr 0x5 @ 10026a2130 (lispobj 10026a20af,pg1236,h=89) sees strange non-pointer
        ; fatal error encountered in SBCL pid 1120122 tid 1120122:
        ; Verify failed: 1 errors
        0
        ))
(dolist (c (sb-vm:list-allocated-objects :all :test #'!system-class-p))
  ;; Missing prototype in ...
  (when (slot-boundp c 'sb-pcl::prototype)
    #+(or) ;;TODO
    (unless (eq 'udef-inttype (class-name c))
    (let ((val (slot-value c 'sb-pcl::prototype)))
      (assert (typep val c))))))
