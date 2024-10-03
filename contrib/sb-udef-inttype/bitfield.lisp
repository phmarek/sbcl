(in-package :sb-udef-inttype)

(defun def-bitfield-struct% (name-and-options slots)
    "Similar to DEFSTRUCT, but all slots must be a (UNSIGNED-BYTE x) type
  and they must fit into 48 bits.

  Notable is that the SETF functions cannot modify the (immediate!) argument,
  so the result of SETF has to be stored!

  Available options:
  - :CONSTRUCTOR
  - :UDEF-INTTYPE-ID
  - :MAX-BITS (how many bits to use to address - 32 means 4G structures max)
  Slot definitions looks like
      (name initial-value :TYPE (UNSIGNED-BYTE length) :MODULO mod)
  but can be abbreviated as
      (name . length)
  If mod is given as T, assignments are restricted to the relevant bits
  (so that INCF won't cause an overflow).
  "
  (destructuring-bind (struct-name &rest options) (sb-int:ensure-list name-and-options)
    (when (zerop (length slots))
      (error "Need at least one slot in ~s" struct-name))
    (flet ((option (name default)
             (or (second (find name options :key #'first)) default)))
      (let* ((udef-inttype-id (option :udef-inttype-id nil))
             (udef-maker (option :udef-maker (gensym "UDEF-MAKER-")))
             (udef-reader (option :udef-reader (gensym "UDEF-READER-")))
             (max-bits (option :max-bits sb-impl::+udef-usable-remaining-bits+))
             (constructor-name (option :constructor
                                       (intern (format nil "~a~a" :make- struct-name)
                                               (symbol-package struct-name)))))
        (loop with pos = 0
              for slot in slots
              for (name init len mod accessor) =
              (cond
                ((numberp (cdr slot))
                 (list (car slot) 0 (cdr slot) nil nil))
                (t
                 (destructuring-bind (name init &key type modulo accessor) slot
                   (unless (and (listp type)
                                (eq (first type) 'unsigned-byte))
                     (error "only (UNSIGNED-BYTE x) types allowed in ~s" struct-name))
                   (list name init (second type) modulo accessor))))
              do (assert (symbolp name))
              do (assert (and (integerp len)
                              (< 0 len sb-impl::+udef-usable-remaining-bits+)))
              do (assert (and (integerp init)
                              (< -1 init (expt 2 len))))
              collect name into slot-names
              collect (or accessor
                          (intern (format nil "~a-~a" struct-name name)
                                  (symbol-package name))) into accessors
              collect init into init-vals
              collect len into lengths
              collect pos into starts
              collect mod into mods
              collect `(byte ,len ,(+ sb-impl::+udef-reserved-low-bits+ pos)) into ldbs
              do (incf pos len)
              when (> pos sb-impl::+udef-usable-remaining-bits+)
              do (error "Structure ~s is too long, only ~d bits available"
                        struct-name sb-impl::+udef-usable-remaining-bits+)
              finally (return
                        `(progn
                           ;;(deftype ,struct-name () 'sb-int:udef-inttype)
                           (sb-impl::def-udef-inttype ,struct-name
                             :id ,udef-inttype-id
                             :max-bits ,max-bits
                             ;; TODO: derive max-bits
                             :constructor ,udef-maker
                             :reader ,udef-reader)
                           ;;
                           (defun ,constructor-name (&key ,@ (mapcar #'list slot-names init-vals))
                             ,@ (loop for n in slot-names
                                      for l in lengths
                                      collect `(check-type ,n (unsigned-byte ,l)))
                             (,udef-maker
                               (logior ,@ (loop for n in slot-names
                                                for p in starts
                                                collect `(ash ,n ,p)))))
                           ;;
                           ,@ (loop for a in accessors
                                    for s in ldbs
                                    ;collect `(declare (inline ,a))
                                    collect `(defun ,a (var)
                                               (check-type var ,struct-name)
                                               (ldb ,s (sb-kernel:get-lisp-obj-address var))))
                           ,@ (loop for a in accessors
                                    for s in ldbs
                                    for l in lengths
                                    for mask = (1- (ash 1 l))
                                    for mod in mods
                                    ;; DEFSETF does (LET* ((#:PLACE input)) ...),
                                    ;; which copies the immediate and makes the SETF meaningless.
                                    ;; So we need DEFINE-SETF-EXPANDER.
                                    collect
                                    `(define-setf-expander ,a (place &environment env)
                                       (multiple-value-bind (temps vals stores store-form access-form)
                                           (get-setf-expansion place env)
                                         (let ((stemp (first stores))
                                               (new (gensym "NEW"))
                                               (old (gensym "OLD")))
                                           (if (cdr stores) (error "Can't expand this."))
                                           (values temps
                                                   vals
                                                   (list new)
                                                   `(let* ((,old ,access-form)
                                                           (,stemp
                                                             (sb-kernel:%make-lisp-obj
                                                               (dpb ,(if ,mod
                                                                         `(logand ,new ,,mask)
                                                                         `(progn
                                                                            (check-type ,new (integer 0 ,,mask))
                                                                            ,new))
                                                                    ,',s
                                                                    (sb-kernel:get-lisp-obj-address ,old)))))
                                                      ,store-form
                                                      ,new)
                                                   `(,',a ,access-form)
                                                   )))))
                           ;;
                           ',struct-name)))))))

(defmacro def-bitfield-struct (name-and-options &rest slots)
  (def-bitfield-struct% name-and-options slots))
