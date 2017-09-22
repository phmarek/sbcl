(in-package :cl-user)

(do-that (make-foo :slot1 42))



#+(or)
(asdf:load-system :foo)
