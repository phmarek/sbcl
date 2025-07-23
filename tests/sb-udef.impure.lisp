(require :sb-udef-inttype)

; (with-compilation-unit () (load "tests/test-util.lisp"))

(test-util:with-scratch-file (f "fasl")
  (load (compile-file
          (merge-pathnames "../../contrib/sb-udef-inttype/example-column-structure.lisp"
                           sb-ext:*runtime-pathname*)
          :output-file f)))

(in-package :col-struct-example)
(use-package :test-util)

(assert (not (typep 4 'sb-kernel::udef-inttype)))
(assert (not (sb-int:udef-inttype-p 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing individual features

(with-test (:name (:udef :foo))
  (setf (foo-b *no-foo*) 12123)
  (assert (= 12123
             (foo-b *no-foo*)))
  (assert (equalp '((:A . ":a") (:B . 12123) (:C . 22))
                  (sb-udef-inttype:column-struct-get-struct *no-foo*)))
  (assert (equalp '(":a" 12123 22)
                  (sb-udef-inttype:with-c-s-slots (foo *no-foo*) (a b c)
                    (list a b c))))
  (setf (foo-b *no-foo*) 1)
  (assert (= 1
             (foo-b *no-foo*))))

;;----------------------------

(with-test (:name (:udef :bar))
  (sb-udef-inttype:column-struct-reset 'bar)
  (with-bar-batch (mmm :batch-size 1000)
    (mmm)
    (make-my-bar-base 40000
                      'loop 0 #(6 4 1) 0 nil nil #() *no-foo*))
  ;; Only a single batch allocated via constructor that cares about numbering
  (assert (= 1000
             (length
               (sb-udef-inttype:c-s-values 'bar))))
  (assert (= 1000
             (let ((c 0))
               (sb-udef-inttype:map-c-s-range
                 (lambda (x)
                   (declare (ignore x))
                   (incf c))
                 'bar)
               c)))
  (assert (= 1000
             (sb-udef-inttype:column-struct-last-index 'bar)))
  ;; But some more got _used_
  (assert (= 42000
             (sb-udef-inttype:column-struct-size 'bar))))

(with-test (:name (:udef :bar-2))
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
                    (bar-vec bar1)))))

;;----------------------------

(make-bcd :digit1 2 :digit2 0 :digit3 2 :digit4 5)

;;----------------------------

(with-test (:name (:udef :var-len-string))
  (sb-udef-inttype:column-struct-reset 'var-len-string)
  (let ((u (make-var-len-string :l 7 :vec "abcdefg")))
    (assert (= 7
               (var-len-string-l u)))
    (assert (string= "abcdefg"
                     (var-len-string-vec u)))
    (setf (var-len-string-vec u :index 1)
          #\X)
    (assert (string= "aXcdefg"
                     (var-len-string-vec u)))
    (setf (var-len-string-vec u :start 1 :end 4)
          "JKLM")
    (assert (string= "aJKLefg"
                     (var-len-string-vec u)))))

;;----------------------------
;; tests CAS

(with-test (:name (:udef :contains-lists))
  (let ((inst (make-contains-lists :l (list 5 6 7))))
    (sb-ext:atomic-push 4 (contains-lists-l inst))
    (assert (equal '(4 5 6 7)
                   (contains-lists-l inst)))))

;;----------------------------

(with-test (:name (:udef :cocoon))
  (sb-udef-inttype:column-struct-reset 'cocoon)
  (let* ((a (make-cocoon))
         (b (make-cocoon :car a))
         (c (make-cocoon :car b :cdr b :cgr b))
         (x (make-cocoon :car a :cdr b :cgr c)))
    (assert (= #xfff0
               (udef/cocoon-operation :tagged-udef-to-int a)))
    (assert (= #xff01
               (udef/cocoon-operation :tagged-udef-to-int b)))
    (assert (= #x1112
               (udef/cocoon-operation :tagged-udef-to-int c)))
    (assert (= #x2103
               (udef/cocoon-operation :tagged-udef-to-int x)))))

;;----------------------------

(with-test (:name (:udef :no-slots))
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
    ;; Test overflow
    (multiple-value-bind (result e)
        (ignore-errors (make-no-slot-udef))
      (assert (null result))
      (assert (typep e 'error)))))


;;----------------------------
;; Batch allocation

(with-test (:name (:udef :batched-var-len-string))
  (sb-udef-inttype:column-struct-reset 'batched-var-len-string)
  (sb-udef-inttype:column-struct-resize 'batched-var-len-string 16)
  (let* ((s1 (make-batched-var-len-string :l 5))
         (s2 (make-batched-var-len-string :l 5))
         (s3 (make-batched-var-len-string :l 8))
         (s4 (make-batched-var-len-string :l 8))
         (counter 0))
    ;; S1 and S2 must be in the same batch
    (assert (= #x500 (udef/batched-var-len-string-operation :tagged-udef-to-int s1)))
    (assert (= #x505 (udef/batched-var-len-string-operation :tagged-udef-to-int s2)))
    ;; S3 in the next, and S4 in another batch
    (assert (= #x80a (udef/batched-var-len-string-operation :tagged-udef-to-int s3)))
    (assert (= #x814 (udef/batched-var-len-string-operation :tagged-udef-to-int s4)))
    (with-bvls (get-bvls
                 :new-batch-cb (lambda (next left)
                                 (declare (ignore next left))
                                 (incf counter)))
      (let ((s5 (get-bvls :l 1)))
        ;; Not implemented yet
        (assert (nth-value 1
                           (ignore-errors (get-bvls :l 3))))
        (assert (= #x11c (udef/batched-var-len-string-operation :tagged-udef-to-int s5)))
        (list s5)))))

;;----------------------------
;; Storage vector types

(with-test (:name (:udef :storage-vector-types))
  (sb-udef-inttype::column-struct-reset 'bar)
  (sb-udef-inttype::column-struct-resize 'bar 1)
  ;;
  (sb-udef-inttype::column-struct-reset 'foo)
  (sb-udef-inttype::column-struct-resize 'foo 1)
  ;;
  ;; Check the *expected* types;
  ;; the actual vector type might be UPGRADED-ARRAY-ELEMENT-TYPE
  (flet ((v-type (c-s slot)
           (let* ((cs (sb-udef-inttype::get-cs-metadata-from-symbol c-s)))
             (sb-udef-inttype::cs-s-storage-type
               (find slot (sb-udef-inttype::cs-meta-slots cs)
                     :key #'sb-udef-inttype::cs-s-slot-name)))))
    ;;
    (assert (equal 'string               (v-type 'foo 'a)))
    (assert (equal '(unsigned-byte 16)   (v-type 'foo 'b)))
    (assert (equal 'fixnum               (v-type 'foo 'c)))
    ;;
    (assert (equal '(unsigned-byte 32)   (v-type 'bar 'i)))
    (assert (equal 'symbol               (v-type 'bar 'name)))
    (assert (equal '(unsigned-byte 32)   (v-type 'bar 'vec)))
    (assert (equal '(unsigned-byte 32)   (v-type 'bar 'typed-ref%)))
    (assert (equal 't                    (v-type 'bar 'ref)))
    (assert (equal '(unsigned-byte 22)   (v-type 'bar 'self)))
    (assert (equal '(unsigned-byte 17)   (v-type 'bar 'udef)))
    ))


;;----------------------------
;; :INDEX-BITS and :MAX-BITS

(with-test (:name (:udef :bits))
  (flet ((bits (sym)
           (multiple-value-bind (cs ud)
               (sb-udef-inttype::get-cs-metadata-from-symbol sym)
             (values (sb-udef-inttype::cs-meta-index-bits cs)
                     (sb-udef-inttype::udef-metadata-max-bits ud)))))
    (assert (=  8 (nth-value 0 (bits 'contains-lists))))
    (assert (= 24 (nth-value 1 (bits 'contains-lists))))
    (assert (=  8 (nth-value 0 (bits 'udef-cons))))
    (assert (= 24 (nth-value 1 (bits 'udef-cons))))
    (assert (=  8 (nth-value 0 (bits 'var-len-string))))
    (assert (= 16 (nth-value 1 (bits 'var-len-string))))
    (assert (=  8 (nth-value 0 (bits 'batched-var-len-string))))))


;;----------------------------
;; High-concurrency resize

(defun thread-do (n &key verbose sem &aux (prev (udef/bar-operation :int-to-tagged-udef 0)))
  (princ
    (format nil "~a starting up~%"
            (sb-thread:thread-os-tid sb-thread:*current-thread*)))
  (when sem
    (sb-thread:wait-on-semaphore sem))
  (flet ((dbg (n c)
           (declare (ignorable n c))
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



(with-test (:name (:udef :concurrent-resize))
  (let ((sem (sb-thread:make-semaphore))
        (per-thread 11270))
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
                      'my-bar-data))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error cases
(assert
  (nth-value 1
             (ignore-errors
               (sb-udef-inttype:def-column-struct (var-len-array-bad1
                                                    (:max-bits 24)
                                                    (:index-bits 16)
                                                    (:batch-size 10)
                                                    (:initial-size 30))
                 (l   0 :type (unsigned-byte 16) :allocation :immediate)
                 (bad nil :type list)
                 (d  22 :type (array fixnum (l)))))))

(assert
  (nth-value 1
             (ignore-errors
               (sb-udef-inttype:def-column-struct (var-len-array-bad2
                                                    (:max-bits 24)
                                                    (:index-bits 16)
                                                    (:batch-size 10)
                                                    (:initial-size 30))
                 (l   0 :type (unsigned-byte 16) :allocation :immediate)
                 (k   0 :type (unsigned-byte 16) :allocation :immediate)
                 (bad 44 :type (array fixnum (k)))
                 (d  22 :type (array fixnum (l)))))))
