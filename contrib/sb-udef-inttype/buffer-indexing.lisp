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


(defun extend-buffer-master (symbol lock new-index make-fn)
  (let ((now (symbol-value symbol)))
    (when (<= (length now)
              new-index)
      (sb-thread:with-mutex (lock)
        (when (<= (length now)
                  new-index)
          (let* ((old (symbol-value symbol))
                 (capacity (+ new-index
                              (min 200
                                   (max (floor (* new-index 0.4))
                                        4))))
                 (contents (concatenate 'list old
                                        (loop for i from (length old) below capacity
                                              collect (funcall make-fn)))))
            #+(or)
            (format t "extend ~s from ~d/~d to ~d~&" symbol
                    (length old)
                    (array-dimension old 0)
                    new-index)
            #+(or)
            (when (and (plusp (length old))
                       (typep (aref old 0)
                              '(array character)))
              (assert (every (lambda (s)
                               (char= (aref s 0)
                                      #\-))
                             old)))
            ;; Required, a simple WHEN leaves wrong data in registers
            ;; for the code below
            (loop while (<= (length (symbol-value symbol))
                            new-index)
                  do (let ((new (make-array (list capacity)
                                            :element-type (array-element-type old)
                                            :initial-contents contents)))
                       (sb-vm:%write-barrier)
                       ;; to leave the loop
                       (setf (symbol-value symbol)
                             new)))
            ;;
            (assert (> (length (symbol-value symbol))
                       new-index))))
        #+(or)
        (format t " alloc ~s ~s got~& ~s~&"
                symbol new-index
                (map 'list #'sb-kernel:get-lisp-obj-address old
                     ))))))


(defmacro make-udef-addressed-buffer (name &key (len-bits 8) (index-bits 24)
                                           (element-type 'character)
                                           (reader-sym (sb-int:symbolicate :GET- name))
                                           (writer-sym (sb-int:symbolicate :SAVE- name))
                                           (reset-sym (sb-int:symbolicate :RESET- name))
                                           (length-sym (sb-int:symbolicate name :-LENGTH))
                                           (pos-sym (sb-int:gensymify* name :-POS))
                                           (maker (sb-int:gensymify* name :-MAKER))
                                           (buffer-sym (sb-int:gensymify* name :-STORAGE))
                                           (lock-sym (sb-int:gensymify* name :-LOCK))
                                           (eob-sym (sb-int:gensymify* name :-EOB))
                                           (initial-size (* 1024 (expt 2 len-bits)))
                                           (batch-size (default-batch-size initial-size))
                                           (initial-element (if (subtypep element-type 'character)
                                                                (code-char 0)
                                                                0))
                                           )
  `(progn
     (def-bitfield-struct
       (,name
         (:constructor ,maker)
         (:max-bits ,(+ len-bits index-bits)))
       (len 0 :type (unsigned-byte ,len-bits)   :accessor ,length-sym)
       (pos 0 :type (unsigned-byte ,index-bits) :accessor ,pos-sym))
  ;;
     (declaim (type sb-vm:word ,eob-sym))
     (defvar ,eob-sym 0)
     (declaim (sb-ext:always-bound ,eob-sym))
     ;;
     (defvar ,buffer-sym (make-array (list 0)
                                     :element-type '(array ,element-type (,batch-size))
                                     :fill-pointer 0
                                     :adjustable t))
     ;;
     (defvar ,lock-sym (sb-thread:make-mutex :name ,(symbol-name name)))
     ;;
     (defun ,reset-sym ()
       (setf ,eob-sym 0)
       (setf ,buffer-sym
             (make-array (list 0)
                         :element-type '(array ,element-type (,batch-size))
                         :fill-pointer 0
                         :adjustable t)))
     ;;
     (declaim (ftype (function (,name) (vector ,element-type)) ,reader-sym))
     (defun ,reader-sym (udef)
       (let* ((p (,pos-sym udef)))
         (multiple-value-bind (outer-idx offset) (floor p ,batch-size)
       (subseq (aref ,buffer-sym outer-idx)
               offset
               (+ offset (,length-sym udef))))))
     ;;
     ;; Do we need to keep unique identity for multiple 0-length elements?
     (defun ,writer-sym (input)
       (let ((len (length input)))
         (flet ((put (x)
                  (multiple-value-bind (idx off) (floor x ,batch-size)
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
                                             (make-array (list ,batch-size)
                                                         :element-type ',element-type
                                                         :initial-element ,initial-element)))
                  ))
           (tagbody
             :retry
             ;; TODO: loop counter and abort?
             (let* ((now ,eob-sym))
               (multiple-value-bind (old-idx old-offset) (floor now ,batch-size)
                 (let ((end (+ old-offset len)))
                   (when (> end ,batch-size)
                     ;; round up to next batch, retry
                     (extend old-idx)
                     (let* ((batch-start (+ now (- ,batch-size old-offset)))
                            (prev (sb-ext:compare-and-swap (symbol-value ',eob-sym)
                                                           now
                                                           (+ batch-start len))))
                       #+(or)
                       (format t "put at ~s in ~s?~&" batch-start ,lock-sym)
                       (if (= prev now)
                           (put batch-start)
                           (go :retry))))
                   ;; enough space in current segment
                   ;; Make sure it exists
                   (when (>= old-idx (length ,buffer-sym))
                     (extend old-idx))
                       #+(or)
                   (format t "put at ~s in ~s?~&" now ,lock-sym)
                   (let* ((prev (sb-ext:compare-and-swap (symbol-value ',eob-sym)
                                                         now
                                                         (+ now len))))
                     (if (= prev now)
                         (put now)
                         (go :retry))))))))))
     ',name))



