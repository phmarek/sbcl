

(apropos "UDEF-INTTYPE")

(describe 'sb-int:udef-inttype-p)
(describe 'sb-int:udef-inttype)
(describe 'sb-int:udef-inttype-value)
(describe 'sb-int:make-udef-inttype)

(assert
  (sb-int:udef-inttype-p
    (sb-kernel:%make-lisp-obj
      (logior (ash 10 8)
              sb-vm::udef-inttype-lowtag))))



#+todo
(defmethod print-object ((x sb-int::udef-inttype) s)
  (print "UDEF" s)
  #+(or)
  (print-unreadable-object (x s :type t)
    nil)
  x
  #+(or)
  (format s "UDEF-0x~x" 1 #+(or)(udef-inttype-value x)))

(prin1-to-string (aref *x* 0))

;; Need to get UDEF-INTTYPE seen as a class, like fixnum
'number
'fixnum
'character
'sb-int:udef-inttype

(dolist (s '(sb-int:udef-inttype 'character 'fixnum))
  (format t "~s: ~s, ~s~%"
          s
          (sb-c::info :type :kind s)
          (sb-c::info :type :builtin s)))


(defclass ordinary ()
  ())
(defstruct ordinary-s ())

'ordinary-s-p

(defmethod my-dispatch ((y ordinary-s))
  :ordinary-s)

(defmethod my-dispatch ((y ordinary))
  :ordinary)

(defmethod my-dispatch ((y fixnum))
  :ordinary)

(my-dispatch :udef-inttype-prototype)
(my-dispatch (make-instance 'ordinary))
(my-dispatch (make-ordinary-s))

;; src/pcl/vector.lisp:814
;;          (LAMBDA ()
;;            (LET* ((SB-PCL::FMF
;;                    (SB-INT:NAMED-LAMBDA (SB-PCL::FAST-METHOD MY-DISPATCH
;;                                          (SB-INT:UDEF-INTTYPE))
;;                        (SB-PCL::.PV. SB-PCL::.NEXT-METHOD-CALL. X)
;;                      (DECLARE (IGNORABLE SB-PCL::.PV. SB-PCL::.NEXT-METHOD-CALL.)
;;                               (DISABLE-PACKAGE-LOCKS SB-PCL::PV-ENV-ENVIRONMENT))
;;                      (DECLARE
;;                       (SB-C::CURRENT-DEFMETHOD MY-DISPATCH NIL (SB-INT:UDEF-INTTYPE)
;;                        (X)))
;;                      (DECLARE (SB-C::SOURCE-FORM (#:***HERE*** (LAMBDA (X) :UDEF))))
;;                      (DECLARE (SB-PCL::%PARAMETER X))
;;                      (DECLARE (TYPE SB-INT:UDEF-INTTYPE X))
;;                      (DECLARE (IGNORABLE X))
;;                      (SYMBOL-MACROLET ((SB-PCL::PV-ENV-ENVIRONMENT SB-PCL::DEFAULT))
;;                        (SB-PCL::FAST-LEXICAL-METHOD-FUNCTIONS ((X)
;;                                                                SB-PCL::.NEXT-METHOD-CALL.
;;                                                                (X) NIL
;;                                                                :CALL-NEXT-METHOD-P NIL
;;                                                                :SETQ-P NIL
;;                                                                :PARAMETERS-SETQD NIL
;;                                                                :METHOD-CELL
;;                                                                (#:METHOD-CELL) :APPLYP
;;                                                                NIL)
;;                          (DECLARE (SB-PCL::%CLASS X SB-INT:UDEF-INTTYPE))
;;                          (LOCALLY
;;                           (DECLARE
;;                            (DISABLE-PACKAGE-LOCKS SB-PCL::%PARAMETER-BINDING-MODIFIED))
;;                           (SYMBOL-MACROLET ((SB-PCL::%PARAMETER-BINDING-MODIFIED
;;                                              (QUOTE)))
;;                             (DECLARE
;;                              (ENABLE-PACKAGE-LOCKS SB-PCL::%PARAMETER-BINDING-MODIFIED))
;;                             (BLOCK MY-DISPATCH :UDEF)))))))
;;                   (SB-PCL::MF (SB-PCL::%MAKE-METHOD-FUNCTION SB-PCL::FMF)))
;;              (SETF (SB-KERNEL:%FUNCALLABLE-INSTANCE-FUN SB-PCL::MF)
;;                      (SB-PCL::METHOD-FUNCTION-FROM-FAST-FUNCTION SB-PCL::FMF
;;                                                                  '(:ARG-INFO (1)
;;                                                                    :CONSTANT-VALUE
;;                                                                    :UDEF)))
;;              SB-PCL::MF))
;;
;;  The value
;;    #<SB-INT:UDEF-INTTYPE #x1>
;;  is not of type
;;    SB-KERNEL:INSTANCE
;;  when binding X
;;     [Condition of type TYPE-ERROR]


;; The problem is
;;                      (DECLARE (TYPE SB-INT:UDEF-INTTYPE X))
#+broken
(progn
  (defmethod my-dispatch ((x sb-int:udef-inttype))
    :udef)
  ;(my-dispatch (sb-int:make-udef-inttype 1))
)

;; make-method-initargs-form-internal
;; make-method-initargs-form-internal1
;; real-make-method-initargs-form
;;
;;Backtrace for: #<SB-THREAD:THREAD tid=2711011 "repl-thread" RUNNING {1000FC8003}>
;; 0: (SB-PCL::MAKE-METHOD-INITARGS-FORM-INTERNAL1 (SB-PCL::SIMPLE-NEXT-METHOD-CALL T SB-PCL::PLIST (:ARG-INFO (1) :CONSTANT-VALUE :UDEF)) ((DECLARE (IGNORABLE X)) (DECLARE (SB-PCL::%CLASS X SB-INT:UDEF-INTTYPE) (TYPE SB-INT:UDEF-INTTYPE X)) (DECLARE (SB-PCL::%PARAMETER X)) (DECLARE (SB-C::SOURCE-FORM (LAMBDA #1=(X) . #2=(:UDEF))) (SB-C::CURRENT-DEFMETHOD MY-DISPATCH NIL (SB-INT:UDEF-INTTYPE) #1#)) (LOCALLY (DECLARE (DISABLE-PACKAGE-LOCKS . #3=(SB-PCL::%PARAMETER-BINDING-MODIFIED))) (SYMBOL-MACROLET ((SB-PCL::%PARAMETER-BINDING-MODIFIED #)) (DECLARE (ENABLE-PACKAGE-LOCKS . #3#)) (BLOCK MY-DISPATCH . #2#)))) (X) ((X) SB-PCL::.METHOD-ARGS. SB-PCL::.NEXT-METHODS. :CALL-NEXT-METHOD-P NIL :SETQ-P NIL :PARAMETERS-SETQD NIL :METHOD-CELL (#:METHOD-CELL) :APPLYP ...) NIL)
;; 1: (SB-PCL::EXPAND-DEFMETHOD MY-DISPATCH #<STANDARD-GENERIC-FUNCTION COMMON-LISP-USER::MY-DISPATCH (1)> #<STANDARD-METHOD {1000556353}> NIL ((X SB-INT:UDEF-INTTYPE)) (:UDEF) #<NULL-LEXENV>)
;; 2: (MACROEXPAND-1 (SB-PCL::%DEFMETHOD-EXPANDER MY-DISPATCH NIL ((X SB-INT:UDEF-INTTYPE)) (:UDEF)) #<NULL-LEXENV>)
;; 3: (MACROEXPAND (SB-PCL::%DEFMETHOD-EXPANDER MY-DISPATCH NIL ((X SB-INT:UDEF-INTTYPE)) (:UDEF)) #<NULL-LEXENV>)
;; 4: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SB-PCL::%DEFMETHOD-EXPANDER MY-DISPATCH NIL ((X SB-INT:UDEF-INTTYPE)) (:UDEF)) #<NULL-LEXENV>)
;;
;; basic-parse-typespec in src/code/type.lisp:1513
; (sb-c::classoid-p *)
; (sb-pcl::classp **)
;(sb-kernel::layout-p ***)

(sb-int:info :type :builtin 'sb-int:udef-inttype)
;; says instance, which would be okay?
;; but which gets in the way during defmethod type checking
(sb-int:info :type :builtin 'fixnum)
;; says #<SB-KERNEL:NUMERIC-TYPE FIXNUM>, which also works for defmethod..
;; src/code/class.lisp:1125

;; (sb-kernel::classoid-non-instance-p 'sb-c::udef-inttype) ;; no classoid

;; Both NIL...
(typep (sb-int:make-udef-inttype 1)
       'sb-kernel:instance)
(typep 1
       'sb-kernel:instance)
;; but (type fixnum y) works out okay

#+(or)
(disassemble
  (sb-mop:method-function
    (find-method #'my-dispatch () (list (find-class 'sb-int:udef-inttype)))))

(assert (eq :anything
            (my-dispatch 1)))

(assert (eq :udef
            (my-dispatch (sb-int:make-udef-inttype 1))))

(disassemble #'my-dispatch)



#+(or)
(ql:quickload :clouseau)

#+(or)
(let ()
  (declare (optimize (debug 3) (speed 1) (safety 3)))
  (let ((fn (compile nil (lambda (x)
                           (declare (optimize (debug 3) (speed 1) (safety 3)))
                           (declare (type sb-int:udef-inttype x))
                           (sb-int:udef-inttype-value x)))))
    ;(clouseau:inspect fn)
    (labels ((hdl (&rest x)
               (error "~s" x))
             (caller ()
             ;(break)
             (handler-bind
                 ((error #'hdl))
             (funcall fn (sb-int:make-udef-inttype (random 1512)))))
             )
      (caller))))


;; doug
(describe
  (aref sb-c::**primitive-object-layouts** sb-int:udef-inttype-lowtag))

(describe
  (sb-c::layout-of (sb-int:make-udef-inttype #x443322)))




;; TODO: unboxing for typed arrays
;; see
(sb-vm::%vector-widetag-and-n-bits-shift '(unsigned-byte 16))
(sb-vm::%vector-widetag-and-n-bits-shift 'sb-int:udef-inttype)

(sb-c::%%typep (sb-int:make-udef-inttype 1) 'sb-int:udef-inttype)
sb-kernel::ctype
(sb-kernel::built-in-classoid-p 'sb-int:udef-inttype)
(sb-kernel::*builtin-classoids*)

sb-kernel::ctype-hashset-insert-if-absent

(defclass udef-meta-class ()
  (
   #+(or)(sb-mop:class-default-initargs :initform ())
   )
  )


(defmethod make-instance ((class udef-meta-class) &key)
  nil)

(defclass my-enum-1 (sb-int:udef-inttype)
  ((id :initform (error "need ID")
       :initargs id
       :type (unsigned-byte 8)))
  (:metaclass udef-meta-class))


(progn
  (eval `(defmethod make-instance ((class symbol) &key)
           nil))
  (trace "SB-PCL")
  (eval `(defmethod make-instance ((class character) &key)
           nil))
  (untrace "SB-PCL"))



;;;;;;;;;;;;;;;;
;;;;
;;;;


(defun to-be-traced (x)
    (sb-unix:unix-close 666)
    (dotimes (i 300000000)
          (type-of
                  (make-my-idx x)))
    (sb-unix:unix-close 667))
;; bpftrace -p 453798 -e 'tracepoint:syscalls:sys_enter_close { if (args->fd == 666) { @enable[tid] = 1; printf("en %d\n", tid); } if (args->fd == 667) { @enable[tid] = 0; } } h:instructions:10 /@enable[tid]/ { printf("%s\n", ustack()) } ' > loga


; TRACE  breaks all the time
(let ((syms '(sb-c::type-of sb-c::primitive-type-of)))
  (eval `(progn
           ;(trace ,@ syms)
           ;(untrace ,@ syms)
           )))

#+(or)
(progn
  (sb-perf:write-jitdump)
  (sb-perf:write-perfmap)
  (sleep 3)
  (to-be-traced #x612))


;;; gdb broken??
;;;;  gdb -p 456920 -ex 'break close if fd==666' -ex "commands" -ex 'fin' -ex 'si' -ex 'end' -ex 'break close if fd == 667' -ex 'commands' -ex 'exit' -ex 'end' -ex 'c' | tee /tmp/46hte
;;;;(sb-thread:thread-os-tid sb-thread:*current-thread*)

(defun to-be-traced (x)
  (sb-unix:unix-close 666)
  (dotimes (i 1 #+(or) 300000000)
    (type-of x
      ))
  (sb-unix:unix-close 667))

(progn
  (sleep 3)
  (to-be-traced (make-my-idx #x151)))
;; yes "si" | gdb -p 455128 -ex 'break close if fd==666' -ex 'c' -ex 'fin' > /tmp/gdb.log



;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(sb-c::layout-of (make-my-idx #x151))

#<SB-KERNEL:LAYOUT {50910A13}>
==============================

The object is a STRUCTURE-OBJECT of type SB-KERNEL:LAYOUT.
FLAGS: 8589934592
CLOS-HASH: 3315979422303135133
CLASSOID: #<SB-KERNEL:BUILT-IN-CLASSOID SB-INT:UDEF-INTTYPE (sealed)>
INVALID: NIL
INHERITS: #(#<SB-KERNEL:LAYOUT for T {50901003}>)
%INFO: NIL
EQUALP-IMPL: 0
SLOT-MAPPER: 0
SLOT-TABLE: #(1 NIL)
ID-WORD0: 0
ID-WORD1: 0
ID-WORD2: 0

(apropos "REGISTER-UDE")


(defclass m34gr (sb-c::ctype) ()
  (:metaclass sb-kernel::structure-class))

(defmethod print-object ((o m34gr) s)
  (print "fafa"))

(defun p-or-not-p (x)
  (values sb-int:udef-inttype-lowtag
          (listp x)
          (sb-int:udef-inttype-p x)))

(format t "~{~a ~}~%"
        (mapcar (lambda (x)
                  (cons x
                        (sb-int:udef-inttype-p
                          (sb-c::%make-lisp-obj x))))
                (alexandria:iota 32)))
(values sb-di::instance-pointer-lowtag
        sb-c::udef-inttype-lowtag)


(type-of (udef-inttype-example::make-my-idx #x151))

(sb-fasl::write-constants-h *standard-output*)

(type-of *data*)
(type-of -1)
(type-of #\3)
