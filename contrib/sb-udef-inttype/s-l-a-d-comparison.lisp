(require :sb-perf)

(require :sb-udef-inttype)

#. (let ((what (second sb-ext:*posix-argv*)))
     (defparameter *def* (if (string= what "1")
                             'defstruct
                             'sb-udef-inttype:def-column-struct))
     (defparameter *name* (cond
                            ((string= what "1")
                             'my-integers)
                             ;; Sized up a few times, reaches *max* more or less exactly.
                            ((string= what "2")
                             '(my-integers (:initial-size 165000)
                                           (:data-var var)))
                            ((string= what "3")
                              '(my-integers (:initial-size 2030000)
                                            (:data-var var)))
                            ((string= what "4")
                              '(my-integers (:initial-size 7000000)
                                            (:batched 1000001)
                                            (:data-var var))))))

#+(or)
(setf *name* '(my-integers (:initial-size 30)
                           (:incf-by 3)
                           (:batched 10)
                           (:data-var var)))

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


(defparameter *max* (or (ignore-errors (parse-integer (sb-posix:getenv "UDEF_MAX")))
                        16000000))


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
(defun check ()
  (assert (eq *data* *data*))
  (assert (not (eq *data* (my-integers-prev *data*))))
  (assert (eq (my-integers-prev *data*) (my-integers-prev *data*)))
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
        finally (assert (null instance))))


(defun sizes ()
  (if (eq *def* 'defstruct)
    (format t "   Created:    ~d~%" (my-integers-v1 *data*))
    (format t "   Used items: ~d of ~d~%"
            (sb-udef-inttype:column-struct-last-index 'my-integers)
            (sb-udef-inttype:column-struct-size 'my-integers))))

(check)
;; Breaks currently because of 60M CONS cells??? UDEF_MAX 13M works.
;(sb-ext:gc :full t)
;(room nil)

(let ((file (third sb-ext:*posix-argv*)))
  (unless (member file '(nil "" "-") :test #'equal)
    (save-lisp-and-die file
                       :executable t
                       :toplevel #'sizes)))

