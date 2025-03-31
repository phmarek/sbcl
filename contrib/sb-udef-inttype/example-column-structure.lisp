(defpackage :col-struct-example
  (:use :cl)
  )
(in-package :col-struct-example)


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
  :to-udef to-bar-udef
  :from-udef from-bar-udef
  :store-udef store-a-bar
  :nil-as-minus-1 t
  :max-bits 32)

(sb-udef-inttype:def-column-struct (bar
                                      (:max-bits 32)
                                      (:batched 8)
                                      (:constructor make-my-bar)
                                      (:base-constructor make-my-bar-base)
                                      (:initial-size 50)
                                      (:batch-size 17)
                                      (:with-batch-allocation-name with-bar-batch)
                                      (:data-var my-bar-data))
  (name :name :type symbol)
  (i 2 :type (unsigned-byte 32))
  (vec 3 :type (array (unsigned-byte 32) (3)))
  (typed-ref% 4 :type (unsigned-byte 32))
  (ref *no-foo* :type T)
  (self nil :type bar)
;;  (self-vec nil :type (array bar (4))) ;; TODO
  (udef *no-foo* :type foo))

#+(or)
(progn
  (sb-udef-inttype:column-struct-reset 'bar)
  (with-bar-batch (mmm :batch-size 20000)
    (mmm)
    (make-my-bar-base (sb-udef-inttype::get-cs-metadata-from-symbol 'bar)
                      40000
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

(defun thread-do (n &key verbose sem &aux (prev (to-bar-udef 0)))
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
  (with-bar-batch (alloc :batch-size 4
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
                       (bar-p s))))
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
  (setf (sb-udef-inttype::cs-meta-batch-size
          (sb-udef-inttype::get-cs-metadata-from-symbol 'bar))
        1000)
  (sb-udef-inttype:column-struct-reset 'bar)
  (sb-udef-inttype:column-struct-size 'bar)
  (sb-udef-inttype:column-struct-last-index 'bar)
  (assert (zerop (sb-udef-inttype:column-struct-last-index 'bar)))
  (loop repeat 40
        collect (sb-thread:make-thread #'thread-do
                                       :arguments (list per-thread :sem sem ))
        into threads
        finally (progn
                  (sb-thread:signal-semaphore sem (length threads))
                  (mapcar #'sb-thread:join-thread threads)))
  (loop with ht = (make-hash-table :test #'eq)
        for i below (sb-udef-inttype:column-struct-last-index 'bar)
        for u = (to-bar-udef i)
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
                                     (:max-bits 19)
                                     (:index-bits 3)
                                     (:initial-size 10)
                                     (:udef-typep foo-p)
                                     (:data-var my-foo-data))
  (a "" :type string)
  (b 0 :type (unsigned-byte 16) :allocation :immediate)
  (c 22 :type fixnum))

(describe
  (aref
    (sb-udef-inttype::cs-meta-slots 
      (get 'imm 'sb-udef-inttype::column-struct-data))
    0))

(defun test-imm (x)
  (sb-udef-inttype::with-c-s-slots (imm x) 
      (a b c)
    (format t "~s ~s ~s~%" a b c)
    (list (string-upcase a)
          b
          (decf c))))
(test-imm (make-imm))
