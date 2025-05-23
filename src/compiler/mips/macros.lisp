;;;; various useful macros for generating MIPS code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB-VM")

;;; Handy macro for defining top-level forms that depend on the compile
;;; environment.

(defmacro expand (expr)
  (let ((gensym (gensym)))
    `(macrolet
         ((,gensym ()
            ,expr))
       (,gensym))))


;;; Instruction-like macros.

(defmacro move (dst src)
  "Move SRC into DST unless they are location="
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst move ,n-dst ,n-src))))
(defmacro emit-nop-or-move (dst src)
  "Move SRC into DST, but if they are location= then emit a NOP"
  (once-only ((n-dst dst)
              (n-src src))
    `(if (location= ,n-dst ,n-src)
         (inst nop)
         (inst move ,n-dst ,n-src))))
(defmacro zeroize (reg)
  `(inst move ,reg zero-tn))

(macrolet ((def-mem-op (op inst shift)
             `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
                `(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag)))))
  (def-mem-op loadw lw word-shift)
  (def-mem-op storew sw word-shift))

(defmacro load-symbol (reg symbol)
  (once-only ((reg reg) (symbol symbol))
    `(inst addu ,reg null-tn (static-symbol-offset ,symbol))))

(defmacro load-symbol-value (reg symbol)
  `(inst lw ,reg null-tn
         (+ (static-symbol-offset ',symbol)
            (ash symbol-value-slot word-shift)
            (- other-pointer-lowtag))))

(defmacro store-symbol-value (reg symbol)
  `(inst sw ,reg null-tn
         (+ (static-symbol-offset ',symbol)
            (ash symbol-value-slot word-shift)
            (- other-pointer-lowtag))))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst lbu ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst lbu ,n-target ,n-source (+ ,n-offset (1- n-word-bytes)))))))


;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions.

(defmacro lisp-jump (function lip)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst addu ,lip ,function (- (ash simple-fun-insts-offset word-shift)
                                   fun-pointer-lowtag))
     (inst j ,lip)
     (emit-nop-or-move code-tn ,function)))

(defmacro lisp-return (return-pc lip &key (offset 0) (frob-code t))
  "Return to RETURN-PC.  LIP is an interior-reg temporary."
  `(progn
     (inst addu ,lip ,return-pc
           (- (* (1+ ,offset) n-word-bytes) other-pointer-lowtag))
     (inst j ,lip)
     ,(if frob-code
          `(emit-nop-or-move code-tn ,return-pc)
          '(inst nop))))


(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (emit-alignment n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
         (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (loadw reg cfp-tn offset))))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
         (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (storew reg cfp-tn offset))))))

(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
              (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
        (sc-case ,n-stack
          ((any-reg descriptor-reg)
           (move ,n-reg ,n-stack))
          ((control-stack)
           (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:
(defmacro with-fixed-allocation ((result-tn flag-tn temp-tn type-code
                                  size
                                  &key (lowtag other-pointer-lowtag))
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
word header having the specified Type-Code.  The result is placed in
Result-TN, Flag-Tn must be wired to NL4-OFFSET, and Temp-TN is a non-
descriptor temp (which may be randomly used by the body.)  The body is
placed inside the PSEUDO-ATOMIC, and presumably initializes the object."
  (unless body
    (bug "empty &body in WITH-FIXED-ALLOCATION"))
  (once-only ((result-tn result-tn) (flag-tn flag-tn) (temp-tn temp-tn)
              (type-code type-code) (size size)
              (lowtag lowtag))
    `(pseudo-atomic (,flag-tn)
       (allocation ,type-code (pad-data-block ,size) ,result-tn ,lowtag
                   (list ,flag-tn ,temp-tn))
       (inst li ,temp-tn (compute-object-header ,size ,type-code))
       (storew ,temp-tn ,result-tn 0 ,lowtag)
       ,@body)))

(defun generate-stack-overflow-check (vop size temp)
  (let ((overflow (generate-error-code vop
                                       'stack-allocated-object-overflows-stack-error
                                       size)))
    #-sb-thread
    (load-symbol-value temp *control-stack-end*)
    #+sb-thread
    (loadw temp thread-base-tn thread-control-stack-end-slot)
    (inst sub temp temp csp-tn)
    (inst sltu temp size temp)
    (inst beq temp overflow)
    (inst nop)))

(defun align-csp (temp1 temp2)
  (inst li temp1 (lognot lowtag-mask))
  (inst addu temp2 csp-tn lowtag-mask)
  (inst and csp-tn temp2 temp1))

;;;; Three Way Comparison
(defun three-way-comparison (x y condition flavor not-p target temp)
  (ecase condition
    (:eq
     (if not-p
         (inst bne x y target)
         (inst beq x y target)))
    (:lt
     (ecase flavor
       (:unsigned
        (inst sltu temp x y))
       (:signed
        (inst slt temp x y)))
     (if not-p
         (inst beq temp target)
         (inst bne temp target)))
    (:gt
     (ecase flavor
       (:unsigned
        (inst sltu temp y x))
       (:signed
        (inst slt temp y x)))
     (if not-p
         (inst beq temp target)
         (inst bne temp target))))
  (inst nop))



;;;; Error Code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    (emit-internal-error kind code values
                         :trap-emitter (lambda (tramp-number)
                                         (inst break 0 tramp-number)))
    (emit-alignment word-shift)))

(defun generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (:elsewhere)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (apply #'error-call vop error-code values)
      start-lab)))


;;;; PSEUDO-ATOMIC

(defmacro pseudo-atomic ((flag-tn &key elide-if (extra nil)) &rest forms)
  (aver (not extra))
  `(progn
     (unless ,elide-if
       (without-scheduling ()
         (store-symbol-value csp-tn *pseudo-atomic-atomic*)))
     ,@forms
     (unless ,elide-if
       (without-scheduling ()
         (store-symbol-value null-tn *pseudo-atomic-atomic*)
         (load-symbol-value ,flag-tn *pseudo-atomic-interrupted*)
         (inst tne zero-tn ,flag-tn)))))

;;;; memory accessor vop generators

(sb-xc:deftype load/store-index (scale lowtag min-offset
                                  &optional (max-offset min-offset))
  `(integer ,(- (truncate (+ (ash 1 16)
                             (* min-offset n-word-bytes)
                             (- lowtag))
                          scale))
            ,(truncate (- (+ (1- (ash 1 16)) lowtag)
                          (* max-offset n-word-bytes))
                       scale)))

(defmacro define-full-reffer (name type offset lowtag scs el-type
                                   &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:temporary (:scs (interior-reg)) lip)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 5
         (inst addu lip object index)
         (loadw value lip ,offset ,lowtag)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset))))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 4
         (loadw value object (+ ,offset index) ,lowtag)))))

(defmacro define-full-setter (name type offset lowtag scs el-type
                                   &optional translate)
  (aver translate)
  `(progn
     (define-vop (,name)
       (:translate ,translate)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg))
              (value :scs ,scs))
       (:arg-types ,type tagged-num ,el-type)
       (:temporary (:scs (non-descriptor-reg)) temp)
       (:vop-var vop)
       (:generator 2
         ,@(if (member name '(instance-index-set %closure-index-set))
               `((without-scheduling ()
                   (emit-gengc-barrier object nil temp (vop-nth-arg 2 vop))
                   (inst addu temp object index)
                   (storew value temp ,offset ,lowtag)))
               `((inst addu temp object index)
                 (storew value temp ,offset ,lowtag)))))
     (define-vop (,(symbolicate name "-C"))
       (:translate ,translate)
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (value :scs ,scs))
       (:info index)
       ,@(when (member name '(instance-index-set %closure-index-set))
           '((:temporary (:scs (non-descriptor-reg)) temp)))
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset)))
                   ,el-type)
       (:vop-var vop)
       (:generator 1
         ,@(if (member name '(instance-index-set %closure-index-set))
               `((without-scheduling ()
                   (emit-gengc-barrier object nil temp (vop-nth-arg 1 vop))
                   (storew value object (+ ,offset index) ,lowtag)))
               `((storew value object (+ ,offset index) ,lowtag)))))))

(defmacro define-partial-reffer (name type size signed offset lowtag scs
                                      el-type &optional translate)
  (let ((scale (ecase size (:byte 1) (:short 2))))
    `(progn
       (define-vop (,name)
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg)))
         (:arg-types ,type positive-fixnum)
         (:results (value :scs ,scs))
         (:result-types ,el-type)
         (:temporary (:scs (interior-reg)) lip)
         (:generator 5
           (inst addu lip object index)
           ,@(when (eq size :short)
               '((inst addu lip index)))
           (inst ,(ecase size
                    (:byte (if signed 'lb 'lbu))
                    (:short (if signed 'lh 'lhu)))
                 value lip (- (* ,offset n-word-bytes) ,lowtag))
           (inst nop)))
       (define-vop (,(symbolicate name "-C"))
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg)))
         (:info index)
         (:arg-types ,type
                     (:constant (load/store-index ,scale
                                                  ,(eval lowtag)
                                                  ,(eval offset))))
         (:results (value :scs ,scs))
         (:result-types ,el-type)
         (:generator 4
           (inst ,(ecase size
                    (:byte (if signed 'lb 'lbu))
                    (:short (if signed 'lh 'lhu)))
                 value object
                 (- (+ (* ,offset n-word-bytes) (* index ,scale)) ,lowtag))
           (inst nop))))))

(defmacro define-partial-setter (name type size offset lowtag scs el-type
                                      &optional translate)
  (let ((scale (ecase size (:byte 1) (:short 2))))
    `(progn
       (define-vop (,name)
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (index :scs (unsigned-reg))
                (value :scs ,scs))
         (:arg-types ,type positive-fixnum ,el-type)
         (:temporary (:scs (interior-reg)) lip)
         (:generator 5
           (inst addu lip object index)
           ,@(when (eq size :short)
               '((inst addu lip index)))
           (inst ,(ecase size (:byte 'sb) (:short 'sh))
                 value lip (- (* ,offset n-word-bytes) ,lowtag))))
       (define-vop (,(symbolicate name "-C"))
         ,@(when translate
             `((:translate ,translate)))
         (:policy :fast-safe)
         (:args (object :scs (descriptor-reg))
                (value :scs ,scs))
         (:info index)
         (:arg-types ,type
                     (:constant (load/store-index ,scale
                                                  ,(eval lowtag)
                                                  ,(eval offset)))
                     ,el-type)
         (:generator 4
           (inst ,(ecase size (:byte 'sb) (:short 'sh))
                 value object
                 (- (+ (* ,offset n-word-bytes) (* index ,scale)) ,lowtag)))))))
