;;;; SB-Perf

(error "Can't build contribs with ASDF")

(defsystem "sb-udef-immediate"
  :components ((:file "package")
               (:file "udef-base")
               (:file "udef-alloc")
               (:file "column-structure")
               ;(:file "bitfield")
               ;(:file "udef-lookup")
               ;(:file "buffer-indexing")
               ;example-column-structure.lisp
               ;example.lisp
               ;simple.lisp
               ;test.lisp
               ))


