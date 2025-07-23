
(defpackage :sb-udef-inttype
  (:documentation "User-defined integer types")
  (:use :common-lisp)
  (:import-from :sb-vm
                #:udef-inttype-widetag)
  (:import-from :sb-int
                #:udef-inttype-type-of
                #:udef-inttype-p
                #:udef-inttype-tag
                #:udef-inttype
                #:*udef-types*
                #:+udef-tag-bits+
                )
  (:export #:def-udef-inttype
           #:udef-inttype-type-of
           #:udef-inttype-p
           #:udef-general-get-value
           #:def-column-struct
           #:column-struct-resize
           #:make-wrapped-udef-accessor
           #:column-struct-reset
           #:column-struct-clear
           #:column-struct-size
           #:column-struct-last-index
           #:column-struct-get-struct

           #:map-c-s-range
           #:c-s-values
           #:with-c-s-slots
         ))

(require :sb-introspect)
