(defpackage :col-struct-example
  (:use :cl)
  )
(in-package :col-struct-example)
#+(or)
(delete-package :col-struct-example)


(require :sb-udef-inttype)

(sb-udef-inttype:def-column-struct (foo
                                     (:max-bits 17)
                                     (:initial-size 10)
                                     (:udef-typep foo-p)
                                     (:data-var my-foo-data))
  (a "" :type string)
  (b 0 :type (unsigned-byte 16))
  (c 22 :type fixnum))

(defparameter *no-foo* (make-foo))

(foo-b *no-foo*)
(sb-udef-inttype:column-struct-get-struct *no-foo*)
(sb-udef-inttype:with-c-s-slots (foo *no-foo*) (a b c)
  (format t "~s ~s ~s~%" a b c))

;(describe my-foo-data)
(let ((x (make-foo :a "1" :b (sb-udef-inttype:column-struct-last-index 'foo)))
      (y (make-foo :a "22" :b 61)))
  (incf (foo-b y))
  (list (foo-a x)
          (foo-b x)
          (foo-a y)
          (foo-b y)
          (sb-udef-inttype::column-struct-last-index 'foo)
          (sb-udef-inttype::column-struct-size 'foo)))


;; Test pre-declaration, for self-referencing stuff
(sb-udef-inttype::def-udef-inttype bar
  :nil-value t
  :max-bits 32)

(sb-udef-inttype:def-column-struct (bar
                                      (:max-bits 32)
                                      (:batched 8)
                                      (:constructor make-my-bar)
                                      (:base-constructor make-my-bar-base)
                                      (:initial-size 50)
                                      (:batch-size 1000)
                                      (:with-batch-allocation-name with-bar-batch)
                                      (:data-var my-bar-data))
  (name :name :type symbol)
  (i 2 :type (unsigned-byte 32))
  (vec 3 :type (array (unsigned-byte 32) (3)))
  (typed-ref% 4 :type (unsigned-byte 32))
  (ref *no-foo* :type T)
  (self nil :type bar)
  (self-vec nil :type (array bar (4))) ;; TODO
  (udef *no-foo* :type foo))

(defun fff (bar1)
  (sb-udef-inttype::with-c-s-slots (bar bar1) (self)
    self))


#+(or)
(progn
  (sb-udef-inttype:column-struct-reset 'bar)
  (with-bar-batch (mmm :batch-size 1000)
    (mmm)
    (make-my-bar-base 40000
                      'loop 0 #(6 4 1) 0 nil nil nil))
  (values (length
            (sb-udef-inttype:c-s-values 'bar))
          (let ((c 0))
            (sb-udef-inttype:map-c-s-range
              (lambda (x)
                (declare (ignore x))
                (incf c))
              'bar)
            c)
          (sb-udef-inttype:column-struct-size 'bar)))

(defparameter *a-bar* (make-my-bar :name 'first
                                   :i 5515
                                   :ref nil
                                   :vec (make-array 3 :element-type '(unsigned-byte 32)
                                                    :initial-contents '(1 2 3))))

(let ((bar1 (make-my-bar
              :name 'second
              :i 12
              :ref nil
              :vec (make-array 3 :element-type '(unsigned-byte 32)
                               :initial-contents '(1 2 3)))))
  (sb-udef-inttype::with-c-s-slots (bar bar1) (vec (i2 i) (i3 i))
    (assert (= i2 i3))
    (incf i2)
    (assert (= i2 i3))
    (setf (aref vec 0)
          17))
  (assert (= 13 (bar-i bar1)))
  (assert (equalp #(17 2 3)
                 (bar-vec bar1))))


(sb-udef-inttype::column-struct-size 'bar)

(sb-udef-inttype:column-struct-last-index 'bar)
(sb-udef-inttype:column-struct-reset 'bar)



;;; Test code

(defun thread-do (n &key verbose sem &aux (prev (udef/bar-operation :int-to-tagged-udef 0)))
  #+(or)
  (princ
    (format nil "~a starting up~%"
            (sb-thread:thread-os-tid sb-thread:*current-thread*)))
  (when sem
    (sb-thread:wait-on-semaphore sem))
  (flet ((dbg (n c)
           #+(or)
           (princ
             (format nil "~a: ~d from ~a~%"
                     (sb-thread:thread-os-tid sb-thread:*current-thread*) c n))))
  (with-bar-batch (alloc :batch-size 7
                     :new-batch-cb #'dbg)
    (dotimes (j n)
      (let ((id (alloc :i j
                       :name :name
                       :self prev
                       :vec (make-array 3 :initial-element j
                                        :element-type '(unsigned-byte 32))
                       :ref (sb-thread:thread-os-tid sb-thread:*current-thread*))))
        (setf prev id)
        (when verbose
          (princ
            (format nil "~s got ~a~%" (bar-ref id) id))))))))

(defun assert-bar-self-is-set (b)
  (let ((v (sb-kernel:get-lisp-obj-address b)))
    ;; Batch allocation leaves holes which are seen as NIL but filled later on
    (unless (and (= #. (logior
                         (ash (sb-udef-inttype::get-existing-udef-id 'bar)
                              8)
                         sb-vm:udef-inttype-lowtag)
                    (logand v #xffff))
                 (let ((s (bar-self b)))
                   (or (null s)
                       (udef/bar-operation :typep s)
                       (error "not a bar"))))
      (error "data broken: ~s  #x~x instead of a BAR"
             b v))))


(defmethod sb-udef-inttype::after-resize-hook ((s (eql 'bar)) cs)
  (assert cs)
  (sb-udef-inttype:map-c-s-range
    #'assert-bar-self-is-set
    cs))


(sb-udef-inttype:column-struct-reset 'bar)

(let ((sem (sb-thread:make-semaphore))
      (per-thread 11270))
  (sb-udef-inttype:column-struct-clear 'bar)
  ;; Quicken allocation a bit
  #+(or)
  (setf (sb-udef-inttype::cs-meta-batch-size
          (sb-udef-inttype::get-cs-metadata-from-symbol 'bar))
        1000)
  (sb-udef-inttype:column-struct-reset 'bar)
  (sb-udef-inttype:column-struct-size 'bar)
  (sb-udef-inttype:column-struct-last-index 'bar)
  (assert (zerop (sb-udef-inttype:column-struct-last-index 'bar)))
  (loop repeat 4
        collect (sb-thread:make-thread #'thread-do
                                       :arguments (list per-thread :sem sem ))
        into threads
        finally (progn
                  (sb-thread:signal-semaphore sem (length threads))
                  (mapcar #'sb-thread:join-thread threads)))
  (loop with ht = (make-hash-table :test #'eq)
        for i below (sb-udef-inttype:column-struct-last-index 'bar)
        for u = (udef/bar-operation :int-to-tagged-udef i)
        do (incf (gethash (bar-ref u) ht 0))
        do (assert-bar-self-is-set u)
        finally
        (return
          (progn
            (unwind-protect
                (loop for k being the hash-key of ht using (hash-value hv)
                      ;; Default value is *no-foo*, ignore these -
                      ;; only thread TIDs are accepted
                      when (integerp k)
                      do (assert (= hv per-thread)))
              (progn 1))
            (values ht
                    'my-bar-data)))))


(sb-udef-inttype:def-column-struct (imm
                                     (:max-bits 24)
                                     (:index-bits 8)
                                     (:initial-size 10)
                                     (:data-var my-imm-data))
  (b 0 :type (unsigned-byte 16) :allocation :immediate)
  (c 22 :type (array fixnum (b))))

(describe
  (aref
    (sb-udef-inttype::cs-meta-slots
      (get 'imm 'sb-udef-inttype::column-struct-data))
    0))

(imm-p 2)
(defun test-imm (x)
  (sb-udef-inttype::with-c-s-slots (imm x)
      (b c)
    (format t "~s ~s~%" b c)
    (list b
          (decf (aref c 0)))))

(test-imm (make-imm :b 2 :c (make-array 2 :element-type 'fixnum
                                        :initial-contents '(111 222))))


(make-imm :b 10 :c (coerce
                     (loop for i from 99
                           repeat 10
                           collect i)
                     '(vector fixnum)))

imm-b
(progn
  (sb-udef-inttype:column-struct-reset 'imm)
  (format nil "~x"
          (sb-kernel:get-lisp-obj-address (make-imm :b #x1234))))

(sb-udef-inttype:def-column-struct (udef-arr
                                     (:index-bits 8)
                                     (:max-bits 12)
                                     (:initial-size 16))
  (l 0 :type (unsigned-byte 4) :allocation :immediate)
  (vec nil :type (array udef-arr (l))))

(let ((u (make-udef-arr :l 7 :vec (vector nil))))
  (format t "~s~%" u)
  (setf (aref (udef-arr-vec u) 2)
        u)
  (setf (udef-arr-vec u :index 4)
        u)
  (sb-udef-inttype:with-c-s-slots (udef-arr u) (l vec)
    (format t "~s ~s~%" l vec)))


(sb-udef-inttype:def-column-struct (udef-cons
                                     (:index-bits 8)
                                     (:max-bits 24)
                                     (:initial-size 16))
  (car nil :type udef-cons :allocation :immediate)
  (cdr nil :type udef-cons :allocation :immediate))

(defvar *leaf-1* (make-udef-cons :car nil :cdr nil))


(sb-udef-inttype:def-column-struct (no-slot-udef
                                     (:max-bits 2)))

(progn
  (sb-udef-inttype:column-struct-reset 'no-slot-udef)
  (let ((a (make-no-slot-udef))
        (b (make-no-slot-udef))
        (c (make-no-slot-udef))
        (d (make-no-slot-udef)))
    (assert (eq a a))
    (assert (not (eq a b)))
    (assert (= 4 (length (remove-duplicates (list a b c d)))))
    ;; TODO: make that return NIL, as per STORE and RETRIEVE?
    ;(assert (null d))
    (multiple-value-bind (result e)
        (ignore-errors (make-no-slot-udef))
      (assert (null result))
      (assert (typep e 'type-error)))))

(sb-udef-inttype:def-column-struct (cocoon
                                     (:index-bits 4)
                                     (:max-bits 48))
  (car  nil    :type cocoon :allocation :immediate)
  (cdr  nil    :type cocoon :allocation :immediate)
  (cgr  nil    :type cocoon :allocation :immediate))

(let ((a (make-cocoon))
      (b (make-cocoon))
      (c (make-cocoon)))
  (make-cocoon :car a :cdr b :cgr c))




(sb-udef-inttype:def-column-struct (bcd
                                     (:max-bits 48))
  (digit1  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit2  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit3  0  :type (unsigned-byte 4)  :allocation :immediate)
  (digit4  0  :type (unsigned-byte 4)  :allocation :immediate))

(make-bcd :digit1 2 :digit2 0 :digit3 2 :digit4 5)
