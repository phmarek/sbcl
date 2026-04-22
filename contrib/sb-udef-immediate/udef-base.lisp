(in-package :sb-udef-immediate)


;; Apart from the bits used by udef-immediate-widetag,
;; this here declares a further
(defconstant +udef-tag-bits+ 8)
;; bits to be used for a range of user-defined integer types.

(defvar *udef-types* nil
  "Vector of user-defined integer types.
  NIL until first use.")
(declaim (type (or null (simple-array symbol (#.(expt 2 +udef-tag-bits+))))
               *udef-types*))

(defun get-existing-udef-id (name &key (start 0))
  (when *udef-types*
    (position name *udef-types*
              :test #'eq
              :start start)))

(defun udef-immediate-tag (x)
  (ldb (byte +udef-tag-bits+ sb-vm:n-widetag-bits)
       (sb-kernel:get-lisp-obj-address x)))

(setf (aref sb-c::**primitive-object-layouts**
            sb-vm:udef-immediate-widetag)
      (sb-kernel:find-layout 'sb-udef-immediate:udef-immediate))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline maybe-udef-check-lambda))
  (defun maybe-udef-check-lambda (plain sub-tag)
    (when (or plain sub-tag)
      `(lambda (object _)
         (declare (ignore _))
         (= (ldb (byte ,(+ (if sub-tag
                               +udef-tag-bits+
                               0)
                           sb-vm:n-widetag-bits)
                       0)
                 (sb-kernel:get-lisp-obj-address object))
            ,(logior sb-vm::udef-immediate-widetag
                     (if sub-tag
                         (ash sub-tag sb-vm:n-widetag-bits)
                         0)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function 'udef-immediate-p)
        (lambda (object)
          (declare (optimize (speed 3) (debug 0)))
          #. (cadddr (maybe-udef-check-lambda t nil)))))

(declaim (ftype (function (T) (values symbol)) udef-immediate-type-of))
(defun udef-immediate-type-of (x)
  "Returns the type symbol, or NIL."
;  (declare (ignore x)) nil
  ;; TODO: error out if not a udef-immediate?
  (when (udef-immediate-p x)
    (let ((type (and *udef-types*
                     (aref *udef-types* (udef-immediate-tag x)))))
      (if type
          type
          'udef-immediate))))


(defun udef-%instance-typep (node)
  (destructuring-bind (object spec) (sb-c::combination-args node)
    (declare (ignore object))
    (or (when (sb-c:constant-lvar-p spec)
          (maybe-udef-check-lambda
                      (eq 'udef-immediate (sb-c:lvar-value spec))
                      (get-existing-udef-id (sb-c:lvar-value spec))))
        (sb-c::give-up-ir1-transform))))

(defun udef-structure-typep (node)
  (destructuring-bind (object type) (sb-c::combination-args node)
    (declare (ignore object))
    (or (when (sb-c:constant-lvar-p type)
          (let* ((udef-layout (load-time-value
                                (sb-c::find-layout 'sb-vm::udef-immediate)))
                 (layout (sb-c:lvar-value type)))
            (maybe-udef-check-lambda
              (eq udef-layout layout)
              (when (find udef-layout
                          (sb-kernel:layout-inherits layout))
                (get-existing-udef-id
                  (sb-kernel::structure-classoid-name
                    (sb-c::layout-classoid layout)))))))
        (sb-c::give-up-ir1-transform))))

;; Due to duplicated argument types we can't use DEFTRANSFORM --
;; that would remove an existing one.
(defun add-a-transform (what where)
  (let ((fi (sb-int:info :function :info where)))
    (unless (member what
                    (sb-c::fun-info-transforms fi)
                    :key #'sb-c::transform-function)
      (push (sb-c::make-transform :type (sb-kernel:specifier-type T)
                                  :function what)
            (sb-c::fun-info-transforms fi)))))

(add-a-transform #'udef-%instance-typep
                 'sb-c::%instance-typep)

(add-a-transform #'udef-structure-typep
                 'sb-c::structure-typep)
