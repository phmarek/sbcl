(in-package :sb-udef-inttype)

;;; The macro MAKE-BUFFER-TYPE handles a large buffer and uses
;;; user-defined integers to address the contents.
;;
;; So callers see a single (64bit) value and can use it like
;; a string or UNSIGNED-BYTE vector, but the actual storage
;; uses only a few lisp objects, which is a great deal faster on GC
;; (and even more on SAVE-LISP-AND-DIE) and,
;; for data that is compact enough (say 8bit length and 24bit index),
;; also much smaller in other data structures.


(defstruct (udef-indexed-buffer-meta
             (:conc-name :udef-i-b-metadata))
  (reader       nil :type symbol)
  (writer       nil :type symbol)
  (reset        nil :type symbol)
  ;;
  (len-bits      -1 :type (unsigned-byte 8))
  (pos-bits      -1 :type (unsigned-byte 8))
  (eql-p        nil :type symbol)
  (len          nil :type symbol)
  (pos          nil :type symbol)
  ;;
  (udef-maker   nil :type symbol)
  (udef-reader  nil :type symbol)
  (buffer       nil :type symbol)
  (eob          nil :type symbol)
  (lock         nil :type symbol)
  ;;
  (dedup-save   nil :type symbol)
  (dedup-retr   nil :type symbol)
  (dedup-data   nil :type symbol)
  (dedup-clear  nil :type symbol)
  (dedup-size   nil :type (or null fixnum))
  ;;
  (batch-size   nil :type (or null fixnum))
  (element-type t   :type t)
  (init-element nil :type t))

(defun extend-buffer-master (symbol lock new-index make-fn)
  (let ((now (symbol-value symbol)))
    (when (<= (length now)
              new-index)
      (sb-thread:with-mutex (lock)
        (sb-vm:without-arena
          ;; TODO: use REDUCE-ELEMENT-COUNTS, ADD-ELEMENTS-TO-VECTOR
          (when (< (length now)
                    new-index)
            (let* ((old (symbol-value symbol))
                   (capacity (+ new-index
                                (min 200
                                     (floor (* new-index 0.4)))))
                   (contents (concatenate 'list old
                                          (loop for i from (length old) below capacity
                                                collect (funcall make-fn)))))
              ;; Required, a simple WHEN leaves wrong data in registers
              ;; for the code below
              (loop while (< (length (symbol-value symbol))
                              new-index)
                    do (let ((new (make-array (list capacity)
                                              :element-type (array-element-type old)
                                              :initial-contents contents)))
                         (sb-vm:%write-barrier)
                         ;; to leave the loop
                         (setf (symbol-value symbol)
                               new)))
              ;;
              (assert (>= (length (symbol-value symbol))
                         new-index)))))))))

;; TODO: allow to reduce index bits by aligning on multiples of 4, 8, or so?

;; TODO: allow &rest further-args for seldom-used stuff?

;; TODO: count? extra or counting index on dedup?
(defmacro make-udef-addressed-buffer (name &key (len-bits 8) (index-bits 24)
                                           (element-type 'character)
                                           (reader-sym (sb-int:symbolicate :GET- name))
                                           (writer-sym (sb-int:symbolicate :SAVE- name))
                                           (reset-sym (sb-int:symbolicate :RESET- name))
                                           ;;
                                           (pos-sym (sb-int:gensymify* name :-POS))
                                           (length-sym (sb-int:symbolicate name :-LENGTH))
                                           (equalp-sym (sb-int:symbolicate name :-EQUALP))
                                           ;;
                                           (maker (sb-int:gensymify* name :-MAKER))
                                           (reader (sb-int:gensymify* name :-READER))
                                           (buffer-sym (sb-int:gensymify* name :-STORAGE))
                                           (eob-sym (sb-int:gensymify* name :-EOB))
                                           (lock-sym (sb-int:gensymify* name :-LOCK))
                                           ;;
                                           (dedup-save-fn nil)
                                           (dedup-retrieval nil)
                                           (dedup-storage-sym (sb-int:gensymify* name :-DEDUP-STORAGE))
                                           (dedup-clear-fn (sb-int:gensymify* name :-DEDUP-CLEAR))
                                           (dedup-size nil)
                                           ;;
                                           (batch-size (* 1024 1024) #+(or)(default-batch-size initial-size))
                                           (initial-element (if (subtypep element-type 'character)
                                                                (code-char 0)
                                                                0))
                                           )
  "Arranges for WRITER-SYM and READER-SYM to store/retrieve vectors
  of ELEMENT-TYPE in a memory/GC-efficient manner,
  by using large vectors and a UDEF to address these.

  INDEX-BITS restricts the total addressable amount of data;
  the per-item length is stored in LEN-BITS (ie. 8 means a
  maximum length of 255), so check your input sizes.

  To avoid storing duplicated data, you might want to use
  DEDUP-SAVE-FN and DEDUP-SIZE; see DEFINE-UDEF-LOOKUP for more details."
  (when (and dedup-save-fn
             (not dedup-size))
    (error "Deduplication wanted but no vector size given"))
  (let ((n-batch-size (eval batch-size))
        (n-dedup-size (eval dedup-size)))
    (assert (typep n-batch-size '(integer 10 10000000)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (def-bitfield-struct
           (,name
             (:constructor ,maker)
             (:reader ,reader)
             (:max-bits ,(+ len-bits index-bits)))
           (len 0 :type (unsigned-byte ,len-bits)   :accessor ,length-sym)
           (pos 0 :type (unsigned-byte ,index-bits) :accessor ,pos-sym))
         (setf (get ',name 'buffer-indexing-data)
               (make-udef-indexed-buffer-meta
                 :reader ',reader-sym
                 :writer ',writer-sym
                 :reset ',reset-sym
                 ;;
                 :len-bits ,len-bits
                 :pos-bits ,index-bits
                 :eql-p ',equalp-sym
                 :len ',length-sym
                 :pos ',pos-sym
                 ;;
                 :udef-maker ',maker
                 :udef-reader ',reader
                 :buffer ',buffer-sym
                 :eob ',eob-sym
                 :lock ',lock-sym
                 ;;
                 :dedup-save ',dedup-save-fn
                 :dedup-retr ',dedup-retrieval
                 :dedup-data ',dedup-storage-sym
                 :dedup-clear ',dedup-clear-fn
                 :dedup-size ',n-dedup-size
                 ;;
                 :batch-size ,n-batch-size
                 :element-type ',element-type
                 :init-element ',initial-element
                 ))
         ;;
         (declaim (type sb-vm:word ,eob-sym))
         (defvar ,eob-sym 0)
         (declaim (sb-ext:always-bound ,eob-sym))
         ;;
         (defparameter ,buffer-sym (make-array (list 0)
                                               :element-type `(simple-array ,',element-type (,,n-batch-size))
                                               :fill-pointer 0
                                               :adjustable t))
         ;;
         (defparameter ,lock-sym (sb-thread:make-mutex :name ,(symbol-name name)))
         ;;
         (declaim (ftype (function (,name) (vector ,element-type)) ,reader-sym))
         (defun ,reader-sym (udef)
           (declare (type ,name udef)) ;; TODO repair -- all are udef types
           (assert (eq ',name (sb-udef-inttype::udef-inttype-type-of udef)))
           (let* ((p (,pos-sym udef)))
             (multiple-value-bind (outer-idx offset) (floor p ,n-batch-size)
               (subseq (aref ,buffer-sym outer-idx)
                       offset
                       (+ offset (,length-sym udef))))))
         ;;
         ;; Do we need to keep unique identity for multiple 0-length elements?
         (defun ,writer-sym (input)
           (let ((len (length input)))
             (flet ((put (x)
                      (multiple-value-bind (idx off) (floor x ,n-batch-size)
                        (when (plusp len)
                          (replace (aref ,buffer-sym idx)
                                   input
                                   :start1 off))
                        (return-from ,writer-sym (,maker :pos x
                                                         :len len))))
                    (extend (i)
                      (extend-buffer-master ',buffer-sym
                                            ,lock-sym
                                            (1+ i)
                                            (lambda ()
                                              (make-array (list ,n-batch-size)
                                                          :element-type ',element-type
                                                          :initial-element ,initial-element)))
                      ))
               (tagbody
                 :retry
                 ;; TODO: loop counter and abort?
                 (let* ((now ,eob-sym))
                   (multiple-value-bind (old-idx old-offset) (floor now ,n-batch-size)
                     (let ((end (+ old-offset len)))
                       (when (> end ,n-batch-size)
                         ;; round up to next batch, retry
                         (extend (1+ old-idx))
                         (let* ((batch-start (+ now (- ,n-batch-size old-offset)))
                                (prev (sb-ext:compare-and-swap (symbol-value ',eob-sym)
                                                               now
                                                               (+ batch-start len))))
                           (if (= prev now)
                               (put batch-start)
                               (go :retry))))
                       ;; enough space in current segment
                       ;; Make sure it exists
                       (when (>= old-idx (length ,buffer-sym))
                         (extend old-idx))
                       (let* ((prev (sb-ext:compare-and-swap (symbol-value ',eob-sym)
                                                             now
                                                             (+ now len))))
                         (if (= prev now)
                             (put now)
                             (go :retry))))))))))
         ;;
         ,@(when dedup-save-fn
             ;; TODO DEACTIVATED
             (sb-int:with-unique-names (saver)
               `((define-udef-lookup ,n-dedup-size ,dedup-retrieval ,saver
                   :table-sym ,dedup-storage-sym
                   :clear-table-fn-sym ,dedup-clear-fn
                   :key ,reader-sym
                   :test ,(if (subtypep element-type 'character)
                              'string=
                              'equalp))
                 (defun ,dedup-save-fn (input)
                   (,saver input
                           (lambda ()
                             (,writer-sym input)))))))
         ;;
         (defun ,equalp-sym (udef seq2 &optional (start2 0) end2)
           (declare (type ,name udef)
                    (type vector seq2) ;; TODO: typecase for list?
                    (type sb-int:index start2)
                    (type (or null sb-int:index) end2)
                    ) ;; TODO repair -- all are udef types
           (assert (eq ',name (sb-udef-inttype::udef-inttype-type-of udef)))
           (let* ((p (,pos-sym udef)))
             (multiple-value-bind (outer-idx offset) (floor p ,n-batch-size)
               (let* ((vec (aref ,buffer-sym outer-idx))
                      (end (or end2 (length seq2)))
                      (end1 (+ offset end (- start2))))
                 (assert (> (length vec) end1))
                 (loop for p upfrom offset below end1
                       for i upfrom start2 below end
                       repeat (,length-sym udef)
                       always (eql (aref vec p)
                                   (aref seq2 i)))))))
         ;;
         (defun ,reset-sym ()
           (setf ,eob-sym 0)
           (setf ,buffer-sym
                 (make-array (list 0)
                             :element-type '(array ,element-type (,n-batch-size))
                             :fill-pointer 0
                             :adjustable t))
           ,(when dedup-save-fn
              `(,dedup-clear-fn))
           ',name))
       ;;
       ',name)))



