(defpackage :col-struct-example
  (:use :cl)
  )
(in-package :col-struct-example)


(require :sb-udef-inttype)

(sb-udef-inttype:def-column-struct (foo
                                     (:max-bits 17)
                                     (:initial-size 10)
                                     (:data-var my-foo-data))
  (a "" :type string)
  (b 0 :type (unsigned-byte 16)))

(defparameter *no-foo* (make-foo))

(foo-b *no-foo*)

(describe my-foo-data)
(let ((x (make-foo :a "1" :b (sb-udef-inttype:column-struct-last-index 'foo)))
      (y (make-foo :a "22" :b 61)))
  (list (foo-a x)
          (foo-b x)
          (foo-a y)
          (foo-b y)
          (sb-udef-inttype::column-struct-size 'foo)
          (sb-udef-inttype::column-struct-size my-foo-data)))


(sb-udef-inttype:def-column-struct (bar
                                      (:max-bits 32)
                                      (:batched 8)
                                      (:udef-maker make-bar-udef)
                                      (:constructor make-my-bar)
                                      (:base-constructor make-my-bar-base)
                                      (:initial-size 5)
                                      (:with-batch-allocation-name with-batch)
                                      (:data-var my-bar-data))
  (name nil :type symbol)
  (i 0 :type (unsigned-byte 32))
  (vec 0 :type (array (unsigned-byte 32) 3))
  (typed-ref% 0 :type (unsigned-byte 32))
  (ref *no-foo* :type T)
  (udef *no-foo* :type foo))


(describe my-bar-data)

(defparameter *a-bar* (make-my-bar :name 'first :i 5515 :ref nil :vec #(1 2 3 4)))


(sb-udef-inttype:make-wrapped-udef-accessor bar-typed-ref bar-typed-ref% bar)

(sb-udef-inttype:column-struct-get-struct *a-bar*)
(setf (bar-typed-ref *a-bar*)
      *a-bar*)
(bar-udef *a-bar*)
(setf (bar-udef *a-bar*)
      (make-foo))

(disassemble 'bar-name)

(sb-udef-inttype::column-struct-size 'bar)
(sb-udef-inttype::column-struct-size my-bar-data)

(sb-udef-inttype:column-struct-last-index 'bar)



;;; Test code

#+(or)
(loop for i = 10000 then (* i 10)
      while (<= i 100e6)
      for b = (sb-udef-inttype::default-batch-size i)
      do (format t "~12d ~12d ~12d~%" i b (floor i b)))


(defun thread-do (n &key verbose sem)
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
  (with-batch (alloc :batch-size 4
                     :new-batch-cb #'dbg)
    (dotimes (j n)
      (let ((id (alloc :i j
             :name :name
             :vec (make-array 3 :initial-element j
                              :element-type '(unsigned-byte 32))
             :ref (sb-thread:thread-os-tid sb-thread:*current-thread*))))
        (when verbose
          (princ
            (format nil "~s got ~a~%" (bar-ref id) id))))))))

(let ((sem (sb-thread:make-semaphore))
      (per-thread 7182))
  (sb-udef-inttype:column-struct-size 'bar)
  (sb-udef-inttype:column-struct-last-index 'bar)
  (sb-udef-inttype:column-struct-clear 'bar)
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
        for u = (make-bar-udef i)
        do (incf (gethash (bar-ref u) ht 0))
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
