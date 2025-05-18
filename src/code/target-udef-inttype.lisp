;;;; User-defined integer types / enumerations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")


;;; High-level interface -- multiple sub-types
;;
;; Apart from the bits used by udef-inttype-lowtag,
;; this here declares a further
(defconstant +udef-tag-bits+ 8)
;; bits to be used for a range of user-defined integer types.


(defconstant +udef-reserved-low-bits+ (+ sb-vm:n-widetag-bits
                                         +udef-tag-bits+))
(defconstant +udef-usable-remaining-bits+ (- sb-vm:n-word-bits
                                             +udef-reserved-low-bits+))


(defvar *udef-types* nil
  "Vector of user-defined integer types.
  NIL until first use.")


(defun get-existing-udef-id (name &key (start 0))
  (when *udef-types*
    (position name *udef-types*
              :test #'eq
              :start start)))

(defun register-udef-subtype-id (type-name &key (random t) preferred)
  "Looks for a free ID in *UDEF-TYPES*,
  and registers TYPE-NAME.
  Not thread-safe."
  (check-type type-name symbol)
  (let ((len (expt 2 +udef-tag-bits+)))
    (unless *udef-types*
      (setf *udef-types* (make-array len
                                     :initial-element nil
                                     :element-type 'symbol)))
    (let ((id (or (get-existing-udef-id type-name)
                  preferred
                  (when random
                    (loop repeat 3
                          for i = (random len)
                          if (null (aref *udef-types* i))
                          return i
                          finally (return
                                    (get-existing-udef-id nil
                                                          :start (random len)))))
                  (get-existing-udef-id nil))))
      (unless id
        (error "too many user-defined integer types in use."))
      (setf (aref *udef-types* id)
            type-name)
      id)))

;; with superclass ctype, (:metaclass sb-kernel::structure-class))
;; #<SIMPLE-ERROR "~S instance constructor called in a non-system file" {1002966753}>
;
;(defclass udef-inttype () ())
;; SB-KERNEL:CTYPE instance constructor called in a non-system file
;;    [Condition of type SIMPLE-ERROR]

(declaim (ftype (function (udef-inttype) (unsigned-byte #. +udef-tag-bits+))
                udef-inttype-tag)
         (inline udef-inttype-tag))
(defun udef-inttype-tag (x)
  ;; TODO: use ldb directly?
  (logand (1- (ash 1 +udef-tag-bits+))
          (udef-inttype-value x)))

(declaim (inline udef-general-get-value))
(defun udef-general-get-value (x)
  (ash (sb-kernel:get-lisp-obj-address x)
       (- (+ sb-vm:n-widetag-bits +udef-tag-bits+))))

(declaim (inline make-twice-tagged-udef)
         (ftype (function ((unsigned-byte #. +udef-tag-bits+)
                           (unsigned-byte #. +udef-usable-remaining-bits+))
                          (values udef-inttype))
                make-twice-tagged-udef))
(defun make-twice-tagged-udef (tag value)
  (sb-kernel:%make-lisp-obj
    (logior (ash value +udef-reserved-low-bits+)
            (logior (ash tag sb-vm:n-widetag-bits)
                    udef-inttype-lowtag))))

(declaim (inline is-tagged-udef-value)
         (ftype (function ((unsigned-byte #. +udef-tag-bits+) T)
                          (values (or (unsigned-byte #. +udef-usable-remaining-bits+)
                                      null)))
                is-tagged-udef-value))

(defun is-tagged-udef-value (tag udef)
  "If UDEF is a UDEF and tagged as TAG, return its value."
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (let* ((v (sb-kernel:get-lisp-obj-address udef))
         (want (logior (ash tag sb-vm:n-widetag-bits)
                       udef-inttype-lowtag))
         (have (ldb (byte +udef-reserved-low-bits+ 0)
                    v)))
    (when (= want have)
      (ldb (byte +udef-usable-remaining-bits+ +udef-reserved-low-bits+)
           v))))

(declaim (inline check-tagged-udef-value)
         (ftype (function ((unsigned-byte #. +udef-tag-bits+)
                           udef-inttype)
                          (values (unsigned-byte #. +udef-usable-remaining-bits+)))
                check-tagged-udef-value))

(defun check-tagged-udef-value (tag udef)
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (or (is-tagged-udef-value tag udef)
      (error "need a udef with tag ~d, not a ~s" tag udef)))

(declaim (ftype (function (T)
                          (values symbol
                                  (unsigned-byte #.(- sb-vm:n-word-bits sb-vm:n-widetag-bits))))
                          udef-inttype-type-of))
(defun udef-inttype-type-of (x)
  "Returns the type symbol, and the content as second value."
  ;; TODO: error out if not a udef-inttype?
  (if (udef-inttype-p x)
    (let ((type (and *udef-types*
                     (aref *udef-types* (udef-inttype-tag x)))))
      (if type
          (values type
                  ;; Possibly we should call using the class' reader slot --
                  ;; but that indirection is much slower than just shifting the bits.
                  ;; (funcall (sb-mop:slot-value-using-class type 'udef-reader) )
                  (udef-general-get-value x))
          (values 'udef-inttype
                  (sb-int:udef-inttype-value x))))
      ;; TODO: (error "not a udef-inttype") ??
      (values nil 0)))

(defstruct udef-metadata
  (udef-id        0 :type (integer 0 255) :read-only t)
  ;; A slot called TYPEP doesn't work with default args in DEFSTRUCT
  (type-p       nil :type symbol          :read-only t)
  (to-udef      nil :type symbol          :read-only t)
  (from-udef    nil :type symbol          :read-only t)
  (max-bits       0 :type (integer 1 48)  :read-only t))

(defun get-existing-udef-f-t-p-b (name)
  "Returns the ID,
  the from-udef, to-udef, and TYPEP functions,
  and the number of bits
  as multiple values if NAME is a defined UDEF."
  (let* ((prev-def% (get name 'udef-metadata))
         (prev-def (when (udef-metadata-p prev-def%)
                        prev-def%)))
    (values (or (and prev-def (udef-metadata-udef-id   prev-def))
                (get-existing-udef-id name))
            (and prev-def (udef-metadata-from-udef prev-def))
            (and prev-def (udef-metadata-to-udef   prev-def))
            (and prev-def (udef-metadata-type-p    prev-def))
            (and prev-def (udef-metadata-max-bits  prev-def)))))

(export '( udef-metadata
           udef-metadata-udef-id
           udef-metadata-from-udef
           udef-metadata-to-udef
           udef-metadata-udef-sym
           udef-metadata-type-p
           udef-metadata-max-bits
           make-twice-tagged-udef
           check-tagged-udef-value
           get-existing-udef-f-t-p-b))

(defmacro def-udef-inttype (name &key id
                                 to-udef from-udef typep
                                 (nil-as-minus-1 t)
                                 (max-bits +udef-usable-remaining-bits+))
  "Defines a new user-defined integer type."
  (multiple-value-bind (old-id)
      (get-existing-udef-f-t-p-b name)
    (let* ((id (cond
                 ((and old-id       id  (= old-id id))  id)
                 ((and (not old-id) id)                 id)
                 (old-id                            old-id)
                 (t
                  (register-udef-subtype-id name))))
           (mask (1- (ash 1 max-bits)))
           ;; TODO: use *PACKAGE* instead of the NAMEs package?
           (typep-fn (or typep
                         (sb-int:symbolicate name :-P)))
           (from-udef-fn (or from-udef
                             (sb-int:symbolicate name :-NUM-FROM-UDEF)))
           (to-udef-fn (or to-udef
                           (sb-int:symbolicate :MAKE- name :-UDEF))))
         ;; (DEBUG 1) is necessary to keep the argument and result types
         `(progn
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (deftype ,name () 'sb-int:udef-inttype)
              ;;
              (assert (= ,id
                         (register-udef-subtype-id ',name :preferred ,id)))
              (unless (get ',name 'udef-metadata)
                (setf (get ',name 'udef-metadata)
                      (make-udef-metadata
                        :udef-id ,id
                        :to-udef  ',to-udef-fn
                        :from-udef ',from-udef-fn
                        :type-p ',typep-fn
                        :max-bits ,max-bits))
                ;; TODO: box/unbox
                (declaim (inline ,typep-fn)
                         (ftype (function (T) (member t nil)) ,typep-fn))
                (defun ,typep-fn (x)
                  (declare (optimize (speed 3) (debug 0) (safety 0)))
                  (when (is-tagged-udef-value ,id x)
                    t))
                ;; From integer (index) to UDEF.
                ;; An incoming NIL gets translated to -1, if so allowed.
                (declaim (inline ,to-udef-fn)
                         (ftype (function ( ,(if nil-as-minus-1
                                                 `(or (integer 0 ,mask)
                                                      null)
                                                 `(integer 0 ,mask)) )
                                          ,name)
                                ,to-udef-fn))
                (defun ,to-udef-fn (x)
                  (declare (optimize (speed 3) (debug 1) (safety 1))
                           (type (or null
                                     (unsigned-byte ,max-bits)) x))
                  (make-twice-tagged-udef ,id
                                          (if (and ,(and nil-as-minus-1 t)
                                                        (eq x nil))
                                                   ,mask
                                                   x)))
                ;; From a (boxed) UDEF to an integer (index).
                ;; A stored -1 may get translated to NIL.
                ;; When initializing a typed slot or array, an incoming
                (declaim (inline ,from-udef-fn)
                         (ftype (function (,name &optional (member t nil))
                                          (values (or (integer 0 ,mask)
                                                      ,@(when nil-as-minus-1
                                                          `(null)) )))
                                ,from-udef-fn))
                ;; When writing integers into specialized arrays,
                ;; we need the -1 representation.
                (defun ,from-udef-fn (x &optional nil-to-minus-1)
                  (declare (optimize (speed 3) (debug 1) (safety 1)))
                  (let ((num (udef-general-get-value x)))
                    (if (and ,(and nil-as-minus-1 t)
                             (not nil-to-minus-1)
                             (= num ,mask))
                        nil
                        num)))))
            ;;
            (values ',name
                    ,id)))))


(defun udef-eq (u1 u2)
  (= (sb-kernel:get-lisp-obj-address u1)
     (sb-kernel:get-lisp-obj-address u2)))

(defun hash-udef (u)
  ;; Do we need equivalence if a type FOO gets removed and
  ;; re-defined with another udef tag?
  ;; TODO: use SXHASH or PSXHASH?
  (sb-kernel:get-lisp-obj-address u))

(sb-ext:define-hash-table-test udef-eq hash-udef)


(defmethod print-object ((obj sb-int:udef-inttype) stream)
  (let* ((v (sb-kernel:get-lisp-obj-address obj))
         (utag (ldb (byte +udef-tag-bits+ sb-vm:n-widetag-bits) v)))
    (format stream "#<UDEF #x~x~@[; tagged #x~x~]~@[, a ~s~]>"
            (ash v (- +udef-reserved-low-bits+))
            utag
            (and *udef-types*
                 (aref *udef-types* utag)))))

;; TODO: (udef-tag-p udef tag) with compiler-macro

;; TODO: subclasses for MOP methods
