(require :sb-udef-immediate)

(eval-when (:compile-toplevel :load-toplevel)
(setf *load-verbose* t))
  (require :sb-posix)
(let ((s (format nil "=== now is ~s~%" (multiple-value-list (get-decoded-time)))))
  (sb-posix:write 1 (sb-alien:make-alien-string s)
                  (length s)))


; (with-compilation-unit () (load "tests/test-util.lisp"))

(test-util:with-scratch-file (f "fasl")
  (load (compile-file
          (merge-pathnames "../../contrib/sb-udef-immediate/example-column-structure.lisp"
                           sb-ext:*runtime-pathname*)
          :output-file f)))

(in-package :col-struct-example)
(use-package :test-util)

(assert (not (typep 4 'sb-udef-immediate::udef-immediate)))
(assert (not (sb-udef-immediate:udef-immediate-type-of 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing individual features

(with-test (:name (:udef :foo))
  (setf (foo-b *no-foo*) 12123)
  (assert (foo-p *no-foo*))
  (assert (= 12123
             (foo-b *no-foo*)))
  (assert (equalp '((:A . ":a") (:B . 12123) (:C . 22))
                  (sb-udef-immediate:column-struct-get-struct *no-foo*)))
  (assert (equalp '(":a" 12123 22)
                  (sb-udef-immediate:with-c-s-slots (foo *no-foo*) (a b c)
                    (list a b c))))
  (setf (foo-b *no-foo*) 1)
  (assert (= 1
             (foo-b *no-foo*))))

;;----------------------------

(with-test (:name (:udef :bar))
  (sb-udef-immediate:column-struct-reset 'bar)
  (sb-udef-immediate:with-batched-cs-allocation (bar mmm :batch-size 1000)
    (mmm)
    (make-my-bar-base 40000
                      'loop 0 #(6 4 1) 0 nil nil #() *no-foo*))
  ;; Only a single batch allocated via constructor that cares about numbering
  (assert (= 1000
             (length
               (sb-udef-immediate:c-s-values 'bar))))
  (assert (= 1000
             (let ((c 0))
               (sb-udef-immediate:map-c-s-range
                 (lambda (x)
                   (declare (ignore x))
                   (incf c))
                 'bar)
               c)))
  (assert (= 1000
             (sb-udef-immediate:column-struct-last-index 'bar)))
  ;; But some more got _used_
  (assert (= 42000
             (sb-udef-immediate:column-struct-size 'bar))))

(with-test (:name (:udef :bar-2))
  (let ((bar1 (make-my-bar
                :name 'second
                :i 12
                :ref nil
                :vec (make-array 3 :element-type '(unsigned-byte 32)
                                 :initial-contents '(1 2 3)))))
    (sb-udef-immediate::with-c-s-slots (bar bar1) (vec (i2 i) (i3 i))
      (assert (= i2 i3))
      (incf i2)
      (assert (= i2 i3))
      (setf (aref vec 0)
            17))
    (assert (= 13 (bar-i bar1)))
    (assert (equalp #(17 2 3)
                    (bar-vec bar1)))))

(with-test (:name (:udef :bar-3))
  (let* ((bar1 (make-my-bar))
         (bar2 (make-my-bar))
         (bart (make-my-bar
                 :name 'third
                 :self-vec (vector bar1 bar2 bar2)))
         (alist (bar-as-alist bart)))
    (assert (equal '(:name . third)
                   (assoc :name alist)))
    (assert (equalp (vector bar1 bar2 bar2 nil)
                   (cdr
                     (assoc :self-vec alist))))
    (assert (eq bar1
                (bar-self-vec bart :index 0)))
    (assert (eq bar2
                (bar-self-vec bart :index 1)))
    (assert (eq bar2
                (bar-self-vec bart :index 2)))
    (assert (eq nil
                (bar-self-vec bart :index 3)))
    ))

;;----------------------------

(with-test (:name (:udef :bcd))
  (assert (= #x5202
             (sb-udef-immediate:udef-general-get-value
               (make-bcd :digit1 2 :digit2 0 :digit3 2 :digit4 5)))))

;;----------------------------

(with-test (:name (:udef :var-len-string))
  (sb-udef-immediate:column-struct-reset 'var-len-string)
  (let* ((abc (make-var-len-string :l 7 :vec "abcdefg"))
         (mno (make-var-len-string :l 5 :vec "mnopq")))
    (assert (= 7
               (var-len-string-l abc)))
    (assert (string= "abcdefg"
                     (var-len-string-vec abc)))
    (setf (var-len-string-vec abc :index 1)
          #\X)
    (assert (string= "aXcdefg"
                     (var-len-string-vec abc)))
    (setf (var-len-string-vec abc :start 1 :end 4)
          "JKLM")
    (assert (string= "aJKLefg"
                     (var-len-string-vec abc)))
    (assert (string= "opq"
                     (var-len-string-vec mno
                                         :start 2
                                         :mode :displaced)))
    (assert (string= "opq"
                     (var-len-string-vec mno
                                         :start 2
                                         :mode :subseq)))
    (assert (equal '((:L . 5) (:VEC . "mnopq"))
                   (var-len-string-as-alist mno)))))

;;----------------------------
;; tests CAS

(with-test (:name (:udef :contains-lists))
  (let ((inst (make-contains-lists :l (list 5 6 7))))
    (sb-ext:atomic-push 4 (contains-lists-l inst))
    (assert (equal '(4 5 6 7)
                   (contains-lists-l inst)))))

;;----------------------------

(with-test (:name (:udef :no-slots))
  (sb-udef-immediate:column-struct-reset 'no-slot-udef)
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
    (dotimes (i 12)
      (make-no-slot-udef))
    (multiple-value-bind (result e)
        (ignore-errors (make-no-slot-udef))
      (assert (null result))
      (assert (typep e 'error)))))

;;----------------------------

(with-test (:name (:udef :cocoon))
  (sb-udef-immediate:column-struct-reset 'no-slot-udef)
  (let ((_0 (make-no-slot-udef))
        (_1 (make-no-slot-udef))
        (_2 (make-no-slot-udef))
        (_3 (make-no-slot-udef)))
      (assert (= 0 (udef/operator-no-slot-udef :tagged-udef-to-int _0)))
      (assert (= 1 (udef/operator-no-slot-udef :tagged-udef-to-int _1)))
      (assert (= 2 (udef/operator-no-slot-udef :tagged-udef-to-int _2)))
      (assert (= 3 (udef/operator-no-slot-udef :tagged-udef-to-int _3)))
    ;;
    (sb-udef-immediate:column-struct-reset 'cocoon)
    (let* ((a (make-cocoon))
           (b (make-cocoon                 :car _0))
           (c (make-cocoon :cgr _1 :cdr _1 :car _1))
           (d (make-cocoon :cgr _3         :car _3))
           (x (make-cocoon :cgr _2 :cdr _1 :car _0)))
      (assert (= #xfff0
                 (udef/operator-cocoon :tagged-udef-to-int a)))
      (assert (= #xff01
                 (udef/operator-cocoon :tagged-udef-to-int b)))
      (assert (= #x1112
                 (udef/operator-cocoon :tagged-udef-to-int c)))
      (assert (= #x3f33
                 (udef/operator-cocoon :tagged-udef-to-int d)))
      (assert (= #x2104
                 (udef/operator-cocoon :tagged-udef-to-int x))))))

;;----------------------------
;; Batch allocation

(with-test (:name (:udef :batched-var-len-string))
  (sb-udef-immediate:column-struct-reset 'batched-var-len-string)
  (sb-udef-immediate:column-struct-resize 'batched-var-len-string 16)
  (let* ((s1 (make-batched-var-len-string :l 5))
         (s2 (make-batched-var-len-string :l 5))
         (s3 (make-batched-var-len-string :l 8))
         (s4 (make-batched-var-len-string :l 8))
         (counter 0))
    ;; S1 and S2 must be in the same batch
    (assert (= #x500 (udef/operator-batched-var-len-string :tagged-udef-to-int s1)))
    (assert (= #x505 (udef/operator-batched-var-len-string :tagged-udef-to-int s2)))
    ;; S3 in the next, and S4 in another batch
    (assert (= #x80a (udef/operator-batched-var-len-string :tagged-udef-to-int s3)))
    (assert (= #x814 (udef/operator-batched-var-len-string :tagged-udef-to-int s4)))
    (sb-udef-immediate:with-batched-cs-allocation
        (batched-var-len-string
          get-bvls
          :new-batch-cb (lambda (next left)
                          (declare (ignore next left))
                          (incf counter)))
      (let ((s5 (get-bvls :l 1)))
        ;; Not implemented yet
        (assert (nth-value 1
                           (ignore-errors (get-bvls :l 3))))
        (assert (= #x11c (udef/operator-batched-var-len-string :tagged-udef-to-int s5)))
        (list s5)))))

;;----------------------------
;; Storage vector types

(with-test (:name (:udef :storage-vector-types))
  (sb-udef-immediate::column-struct-reset 'bar)
  (sb-udef-immediate::column-struct-resize 'bar 1)
  ;;
  (sb-udef-immediate::column-struct-reset 'foo)
  (sb-udef-immediate::column-struct-resize 'foo 1)
  ;;
  ;; Check the *expected* types;
  ;; the actual vector type might be UPGRADED-ARRAY-ELEMENT-TYPE
  (flet ((v-type (c-s slot)
           (sb-udef-immediate::slot-storage-type
             (sb-udef-immediate::find-slot-by-name c-s slot))))
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
               (sb-udef-immediate::get-cs-metadata-from-symbol sym)
             (values (sb-udef-immediate::cs-meta-index-bits cs)
                     (sb-udef-immediate::udef-metadata-max-bits ud)))))
    (assert (=  8 (nth-value 0 (bits 'contains-lists))))
    (assert (= 24 (nth-value 1 (bits 'contains-lists))))
    (assert (=  8 (nth-value 0 (bits 'udef-cons))))
    (assert (= 24 (nth-value 1 (bits 'udef-cons))))
    (assert (=  8 (nth-value 0 (bits 'var-len-string))))
    (assert (= 16 (nth-value 1 (bits 'var-len-string))))
    (assert (=  8 (nth-value 0 (bits 'batched-var-len-string))))))


;;----------------------------
;; High-concurrency resize

(defun thread-do (n &key verbose sem &aux (prev (udef/operator-bar :int-to-tagged-udef 0)))
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
  (sb-udef-immediate:with-batched-cs-allocation (bar alloc :batch-size 7
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
                         (ash (sb-udef-immediate::get-existing-udef-id 'bar)
                              8)
                         sb-vm:udef-immediate-widetag)
                    (logand v #xffff))
                 (let ((s (bar-self b)))
                   (or (null s)
                       (udef/operator-bar :typep s)
                       (error "not a bar"))))
      (error "data broken: ~s  #x~x instead of a BAR"
             b v))))


(defmethod sb-udef-immediate::after-resize-hook ((s (eql 'bar)) cs)
  (assert cs)
  (sb-udef-immediate:map-c-s-range
    #'assert-bar-self-is-set
    cs))



(with-test (:name (:udef :concurrent-resize))
  (let ((sem (sb-thread:make-semaphore))
        (per-thread 11270)
        (thread-count 40))
    (sb-udef-immediate:column-struct-reset 'bar)
    (sb-udef-immediate:column-struct-size 'bar)
    (sb-udef-immediate:column-struct-last-index 'bar)
    (assert (zerop (sb-udef-immediate:column-struct-last-index 'bar)))
    (loop repeat thread-count
          collect (sb-thread:make-thread #'thread-do
                                         :arguments (list per-thread :sem sem ))
          into threads
          finally (progn
                    (sb-thread:signal-semaphore sem (length threads))
                    (mapcar #'sb-thread:join-thread threads)))
    (loop with ht = (make-hash-table :test #'eq)
          for i below (sb-udef-immediate:column-struct-last-index 'bar)
          for u = (udef/operator-bar :int-to-tagged-udef i)
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


(let ((x 0))
  (defun next-index ()
    (logand #xffffff
            (incf x))))

(sb-udef-immediate:make-wrapped-udef-accessor next-udef next-index foo)

(with-test (:name (:udef :wrapped-accessor))
  (assert (eql 1
               (next-index)))
  (assert (eql (udef/operator-foo :int-to-tagged-udef 2)
               (next-udef)))
  (assert (eql 3
               (udef/operator-foo :udef-or-nil-to-ub-x (next-udef))))
  (assert (eql (udef/operator-foo :int-to-tagged-udef 4)
               (next-udef)))
  (assert (eql 5
               (next-index))))

(with-test (:name (:udef :typep))
  (assert (typep *no-foo* 'foo)))

#+(or) ; TODO Broken
(with-test (:name (:udef :type-of))
  (assert (eq (type-of (make-twice-tagged-udef
                         (position nil *udef-types*)
                         1))
              'udef-immediate)))

#+(or) ; TODO Broken
(with-test (:name (:udef :type-of))
  (assert (eq (type-of *no-foo*) 'foo)))


(defgeneric udef-gen-fn (x)
  (:method ((x fixnum))         :fixnum)
  (:method ((x foo))            :foo)
  (:method ((x udef-immediate)) :udef)
  (:method ((x bar))            :bar)
  (:method ((x (eql *no-foo*))) :no-foo))

(with-test (:name (:udef :defmethod))
  (assert (eq (udef-gen-fn 5)                   :fixnum))
  (assert (eq (udef-gen-fn (make-foo))          :foo))
  (assert (eq (udef-gen-fn (make-no-slot-udef)) :udef))
  (assert (eq (udef-gen-fn (make-my-bar))       :bar))
  #+(or) ; TODO
  (assert (eq (udef-gen-fn *no-foo*)            :no-foo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error cases
(assert
  (nth-value 1
             (ignore-errors
               (sb-udef-immediate:def-column-struct (var-len-array-bad1
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
               (sb-udef-immediate:def-column-struct (var-len-array-bad2
                                                    (:max-bits 24)
                                                    (:index-bits 16)
                                                    (:batch-size 10)
                                                    (:initial-size 30))
                 (l   0 :type (unsigned-byte 16) :allocation :immediate)
                 (k   0 :type (unsigned-byte 16) :allocation :immediate)
                 (bad 44 :type (array fixnum (k)))
                 (d  22 :type (array fixnum (l)))))))
