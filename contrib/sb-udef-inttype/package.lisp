
(defpackage :sb-udef-inttype
  (:use :common-lisp)
  (:import-from :sb-impl
                #:def-udef-inttype
                #:udef-metadata
                #:udef-metadata-udef-reader
                #:udef-metadata-udef-maker
                #:udef-metadata-max-bits
                #:udef-inttype-type-of
                )
  (:export #:def-column-struct
           #:column-struct-resize
           #:make-wrapped-udef-accessor
           #:column-struct-reset
           #:column-struct-clear
           #:column-struct-size
           #:column-struct-last-index
           #:column-struct-get-struct

           #:def-bitfield-struct

           #:make-udef-addressed-buffer
         ))

(require :sb-introspect)
