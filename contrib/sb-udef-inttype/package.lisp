
(defpackage :sb-udef-inttype
  (:use :common-lisp)
  (:import-from :sb-impl
                #:udef-inttype-type-of
                #:udef-inttype-p
                #:udef-inttype-value
                #:udef-inttype-lowtag
                #:udef-inttype-tag
                #:udef-inttype
                #:*udef-types*
                )
  (:export #:def-udef-inttype
           #:udef-inttype-type-of
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
