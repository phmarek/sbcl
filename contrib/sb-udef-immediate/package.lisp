
(defpackage :sb-udef-immediate
  (:documentation "User-defined integer types")
  (:use :common-lisp)
  (:import-from :sb-vm
                #:udef-immediate
                #:udef-immediate-widetag)
  (:export #:udef-immediate-type-of
           #:udef-immediate-widetag
           #:udef-immediate-tag
           #:udef-immediate
           #:*udef-types*
           #:+udef-tag-bits+
           #:def-udef-immediate
           #:udef-immediate-p
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
           #:with-batched-cs-allocation
           ))

(require :sb-introspect)
