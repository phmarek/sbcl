(require :sb-perf)

(require :sb-udef-inttype)

(defparameter *max* (or (ignore-errors (parse-integer (sb-posix:getenv "UDEF_MAX")))
                        14000000))

#. (let ((what (or (second sb-ext:*posix-argv*)
                   "2")))
     (defparameter *def* (if (string= what "1")
                             'defstruct
                             'sb-udef-inttype:def-column-struct))
     (defparameter *name* (cond
                            ((string= what "1")
                             `my-integers)
                             ;; Sized up a few times, reaches *max* more or less exactly.
                            ((string= what "2")
                             `(my-integers (:index-bits 32)
                                           (:initial-size 165000)))
                            ((string= what "3")
                              `(my-integers (:index-bits 32)
                                            (:initial-size ,(round (* 1.1 *max*)))))
                            ((string= what "4")
                              `(my-integers (:index-bits 32)
                                            (:initial-size 1000001)
                                            (:batch-size T)))))
     (defparameter *stg-type* (if (string= what "1")
                                  '(string 8)
                                  '(array character (8))))
     (defparameter *ref*  (if (string= what "1")
                             '(or null my-integers)
                              'my-integers)))

#+(or)
(defmethod sb-udef-inttype::after-resize-hook (sym c-s)
  (format *trace-output* "RESIZE ~s to ~d~%"
          sym
          (sb-udef-inttype::cs-meta-allocated c-s)))


(#. *def*  #. *name*
  (prev  nil :type #. *ref*)
  (pprev nil :type #. *ref*)
  (name  #\- :type #. *stg-type*)
  (v1      0 :type (unsigned-byte 32))
  (v2      0 :type (unsigned-byte 16)))
#+(or)
(let ((f '1
))

  #+(or)
(print
  (macroexpand f))
  (eval f))

;; Only now, to have the accessors available
(sb-perf:write-perfmap)

#+(or)
(unless (eq *def* 'defstruct)
  (format t "Start size: ~d of ~d~%"
          (sb-udef-inttype::column-struct-last-index 'my-integers)
          (sb-udef-inttype::column-struct-size 'my-integers)))


#+(or)
(print
  (macroexpand
'(SB-UDEF-INTTYPE::EXPAND-C-S-DEFINITION my-integers
                                        make-my-integers
                                        mmi-constructor NIL)
)
)
#+(or)
(let* ((a (make-my-integers :prev nil :pprev nil :name "12345678"))
       (b (make-my-integers :prev a)))
  (make-my-integers :prev b :pprev a))


(defparameter *data* (loop for i from 0 to *max*
                           for pprev = nil then prev
                           for prev  = nil then instance
                           for name  = (format nil "~8,'0x" i)
                           for instance = (make-my-integers :prev prev
                                                            :pprev pprev
                                                            :v1 i
                                                            :name name)
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
        do (let ((type (sb-udef-inttype:udef-inttype-type-of instance))
                 (raw  (sb-udef-inttype:udef-general-get-value instance)))
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
    (format t "      Created:    ~d~%" (my-integers-v1 *data*))
    (format t "      Used items: ~d of ~d~%"
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
                       :toplevel #'sizes
                       :save-runtime-options t
                       )))

