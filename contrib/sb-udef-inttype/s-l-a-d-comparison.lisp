

(require :sb-perf)

(require :sb-udef-inttype)

#. (let ((what (second sb-ext:*posix-argv*)))
     (defparameter *def* (if (string= what "1")
                             'defstruct
                             'sb-column-struct:def-column-struct))
     (defparameter *name* (cond
                            ((string= what "3")
                             ;; A size that, sized up a few times, reaches 10M.
                              '(my-integers (:initial-size 3568000)))
                            ((string= what "4")
                              '(my-integers (:initial-size 3000000)
                                            (:batched 1000000)))
                            (t ; 1 & 2
                              'my-integers))))

#+(or)
(setf *name* '(my-integers (:initial-size 30)
                           (:batched 10)
                           (:data-var mymy)))

(#. *def*  #. *name*
  (prev nil :type t)
  (v1 0 :type fixnum)
  (v2 0 :type fixnum))

;; Only now, to have the accessors available
(sb-perf:write-perfmap)

#+(or)
(unless (eq *def* 'defstruct)
  (format t "Start size: ~d of ~d~%"
          (sb-column-struct:column-struct-last-index 'my-integers)
          (sb-column-struct:column-struct-size 'my-integers)))


(defparameter *max* 20000000)


(defparameter *data* (loop for i from 0 to *max*
                           for instance = (make-my-integers :prev instance
                                                            :v1 i)
                           do (setf (my-integers-v2 instance)
                                    (mod i 37))
                           finally (return instance)))

;; TODO: broken
#+(or)
(unless (eq *def* 'defstruct)
  (describe *data*))

;; check content
(loop with instance = *data*
      for i from *max* downto 0
      unless (eq *def* 'defstruct)
      do (multiple-value-bind (type raw) 
             (sb-impl::udef-inttype-type-of instance)
           (assert (eq type 'my-integers))
           (assert (= raw i)))
      ;;
      do (assert (= (my-integers-v1 instance)
                    i))
      do (assert (= (my-integers-v2 instance)
                    (mod i 37)))
      do (setf instance (my-integers-prev instance))
      ;if (zerop (mod i 100000))
      ;do (format t "   okay for ~d~%" i)
      finally (assert (null instance)))


#+(or)
(unless (eq *def* 'defstruct)
  (format t "Final size: ~d of ~d~%"
          (sb-column-struct:column-struct-last-index 'my-integers)
          (sb-column-struct:column-struct-size 'my-integers)))

(let ((file (third sb-ext:*posix-argv*)))
  (when (string/= "-" file)
    (save-lisp-and-die file :executable t)))

