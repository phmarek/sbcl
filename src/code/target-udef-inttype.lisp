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


(defvar *udef-types* nil
  "Vector of user-defined integer types.
  NIL until first use.")

(declaim (ftype (function (T) (values symbol)) udef-inttype-type-of))
(defun udef-inttype-type-of (x)
  "Returns the type symbol, and the content as second value."
  ;; TODO: error out if not a udef-inttype?
  (if (udef-inttype-p x)
      (let ((type (and *udef-types*
                       (aref *udef-types* (udef-inttype-tag x)))))
        (if type
            type
            'udef-inttype))
      ;; TODO: (error "not a udef-inttype") ??
      (values nil 0)))
