(defpackage :buffer-index-example
  (:use :cl))

(use-package :buffer-index-example)

(require :sb-udef-inttype)

  (setf (extern-alien "pre_verify_gen_0" int) 1)
  (setf (extern-alien "verify_gens" char) 0)

(sb-udef-inttype:make-udef-addressed-buffer blobs
                                                    :len-bits 4
                                                    :index-bits 12
                                                    :element-type (unsigned-byte 8)
                                                    :reader-sym get-blob
                                                    :writer-sym save-blob
                                                    :reset-sym reset-blobs
                                                    :length-sym blob-length
                                                    :buffer-sym blob-data
                                                    :initial-size 32
                                                    :batch-size 16)

(reset-blobs)
(loop for i below 15
      collect (save-blob (make-array i
                                     :element-type '(unsigned-byte 8)
                                     :initial-element i)) into addr
      finally (return (loop for a in addr
                            collect (get-blob a))))



(sb-udef-inttype:make-udef-addressed-buffer strings
                                                    :len-bits 8
                                                    :index-bits 32
                                                    :element-type character
                                                    :reader-sym get-stg
                                                    :writer-sym save-stg
                                                    :reset-sym reset-stgs
                                                    :length-sym stg-length
                                                    :buffer-sym stg-data
                                                    :maker stg-udef-maker
                                                    :initial-size 0
                                                    :batch-size 32768)

(defun thread-do (me n &key sem)
  (when sem
    (sb-thread:wait-on-semaphore sem))
  (loop with base = (* me n)
        for i below n
        collect (save-stg (format nil "-~d " (+ i base)))))

(sb-ext:gc :full t)
(defparameter *stgs*
  (let ((sem (sb-thread:make-semaphore))
        (per-thread 40000)
        (thread-count 40))
    (reset-stgs)
    (loop for i below thread-count
          collect (sb-thread:make-thread #'thread-do
                                         :arguments (list i per-thread :sem sem))
          into threads
          finally (return
                    (progn
                      (sb-thread:signal-semaphore sem thread-count)
                      (let ((vals (mapcan #'sb-thread:join-thread threads)))
                        (format t "expected ~d vals, got ~d~%"
                                (* per-thread thread-count)
                                (length vals))
                        (return vals)))))))

#+(or)
(identity stg-data)
#+(or)
(get-stg (stg-udef-maker :len 4  :pos #x1a4))

(format t "doing gc...~%")
(sb-ext:gc :full t)
(room)

;; Verification
(let* ((stgs (loop for a in *stgs*
                   for i upfrom 0
                   for l = (stg-length a)
                   for s = (get-stg a)
                   for v = (handler-case
                               (- (parse-integer s))
                             (error (e)
                               (declare (ignore e))
                               (error "can't read from ~s, got ~s" a s)))
                  collect (cons v a)))
      (sorted (sort stgs #'<
                    :key #'car)))
  (loop for i from 0
        for (v . a) in sorted
        do (assert (= i v)
                   ()
                   "Value ~s wrong, from ~s" v a)
        finally (format t "got ~d correct~&" i)))
