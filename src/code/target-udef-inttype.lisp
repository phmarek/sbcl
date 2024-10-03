;;;; User-defined integer types / enumerations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INT")

;; Apart from the bits used by udef-inttype-widetag,
;; this here declares a further
(defconstant +udef-tag-bits+ 8)
;; bits to be used for a range of user-defined integer types.

(defvar *udef-types* nil
  "Vector of user-defined integer types.
  NIL until first use.")

(declaim (inline udef-inttype-p))

(defun sb-int::udef-inttype-p (x)
  (= sb-vm::udef-inttype-widetag
     (ldb (byte sb-vm:n-widetag-bits 0)
          (sb-kernel:get-lisp-obj-address x))))

;; This FTYPE gets me an error
;;   0: (ALLOCATE-CONDITION NIL)
;;   1: (MAKE-CONDITION NIL)
;;   2: (MAKE-CONDITION NIL) [more]
;;   3: (COERCE-TO-CONDITION NIL SIMPLE-ERROR ERROR) [more]
;;   4: (ERROR NIL)
;;   5: ((LABELS SB-KERNEL::RECURSE :IN CTYPEP) NIL #<BUILT-IN-CLASSOID UDEF-INTTYPE (sealed)>)
;;   6: ((LAMBDA (ELT) :IN SB-KERNEL::MEMBER-COMPLEX-SUBTYPEP-ARG1-TYPE-METHOD) NIL)
;;   7: (MAP-XSET #<FUNCTION (LAMBDA (ELT) :IN SB-KERNEL::MEMBER-COMPLEX-SUBTYPEP-ARG1-TYPE-METHOD) {12071F718B}> #S(XSET :DATA (NIL) :EXTRA 1))
;;   8: (SB-KERNEL::MEMBER-COMPLEX-SUBTYPEP-ARG1-TYPE-METHOD #<MEMBER-TYPE NULL> #<BUILT-IN-CLASSOID UDEF-INTTYPE (sealed)>)
;;   9: (CSUBTYPEP #<MEMBER-TYPE NULL> #<BUILT-IN-CLASSOID UDEF-INTTYPE (sealed)>)
;;   10: (SB-KERNEL::%COERCE-TO-VALUES #<BUILT-IN-CLASSOID UDEF-INTTYPE (sealed)>)
;;   11: (SB-KERNEL::SIMPLIFY-UNIONS (#<MEMBER-TYPE NULL> #<BUILT-IN-CLASSOID UDEF-INTTYPE (sealed)>))
;;   12: (SB-KERNEL::%TYPE-UNION (#<MEMBER-TYPE NULL> #<BUILT-IN-CLASSOID UDEF-INTTYPE (sealed)>))
;;   13: (TYPE-UNION #<MEMBER-TYPE NULL> #<BUILT-IN-CLASSOID UDEF-INTTYPE (sealed)>) [more]
;;   14: (SB-C::IR1-TRANSFORM-TYPE-PREDICATE #<SB-C::LVAR 1 {1207284523}> #<BUILT-IN-CLASSOID UDEF-INTTYPE (sealed)> #<SB-C::COMBINATION :FUN #<SB-C::REF  :LEAF #<SB-C::GLOBAL-VAR :%SOURCE-NAME UDEF-INTTYPE-P :TYPE #1=#<FUN-TYPE (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))> :DEFINED-TYPE #1# :WHERE-FROM :DECLARED :KIND :GLOBAL-FUNCTION {12072842F3}> {12072843D3}> :ARGS (#<SB-C::REF :%SOURCE-NAME X :LEAF #<SB-C::LAMBDA-VAR :%SOURCE-NAME X {1207283803}> {1207284573}>) {1207284443}>)
;; during cold/warm.
(declaim #+(or)(ftype (function (udef-inttype) (unsigned-byte #. +udef-tag-bits+))
                udef-inttype-tag)
         (inline udef-inttype-tag))
(defun udef-inttype-tag (x)
  (ldb (byte +udef-tag-bits+ sb-vm:n-widetag-bits)
       (sb-kernel:get-lisp-obj-address x)))

(declaim (ftype (function (T) (values symbol)) udef-inttype-type-of))
(defun udef-inttype-type-of (x)
  "Returns the type symbol, or NIL."
;  (declare (ignore x)) nil
  ;; TODO: error out if not a udef-inttype?
  (when (udef-inttype-p x)
    (let ((type (and *udef-types*
                     (aref *udef-types* (udef-inttype-tag x)))))
      (if type
          type
          'udef-inttype))))
