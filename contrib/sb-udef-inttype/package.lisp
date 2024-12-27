
(defpackage :sb-udef-inttype
  (:use :common-lisp)
  (:import-from :sb-impl
                #:def-udef-inttype
                #:udef-inttype-type-of
                #:udef-metadata
                #:udef-metadata-from-udef
                #:udef-metadata-to-udef
                #:udef-metadata-store-udef
                #:udef-metadata-retr-udef
                #:udef-metadata-type-p
                #:udef-metadata-max-bits
                #:get-existing-udef-f-t-p-b
                #:get-existing-udef-id
                )
  (:export #:def-udef-inttype
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

           #:def-bitfield-struct

           #:make-udef-addressed-buffer
         ))

(require :sb-introspect)
