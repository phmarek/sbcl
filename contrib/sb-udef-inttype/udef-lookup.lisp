(in-package :sb-udef-inttype)


;;; This macro provides a memory-efficient lookup table --
;;; like a hash-table, but non-resizing.
;;
;; Basically, some value's hash is used as an index in a vector
;; to store a UDEF.



(defun udef-lookup-index (input table-size hash-value)
  (let* ((hash (or hash-value
                   (sb-int:psxhash  ;; TODO: configurable?
                     (etypecase input
                       ((or string base-string array)
                        input)
                       (sb-c::udef-inttype
                         (sb-kernel:get-lisp-obj-address input)))))))
    (mod hash table-size)))

(defmacro define-udef-lookup (table-size retrieve-fn-sym save-fn-sym &key
                                         (key (error "Need a KEY to get a comparison value from a UDEF"))
                                         (test 'equalp)
                                         clear-table-fn-sym
                                         (udef-type 'sb-c::udef-inttype)
                                         (table-sym (gensym "LOOKUP")))
  "SAVE-FN-SYM gets defined to be a function of one argument
  (which should be a string or (array (unsigned-byte)));
  if the given argument cannot be found via TEST and KEY,
  the new UDEF to be stored is built by running the BODY.
  RETRIEVE-FN-SYM can be used for lookup without inserting data."
  (sb-int:with-unique-names (lookup size)
    `(progn
       (defconstant ,size ,table-size)
       (defparameter ,table-sym
         (make-array (list ,size)
                     :element-type '(or (vector ,udef-type)
                                        (cons ,udef-type)
                                        (cons (vector ,udef-type))
                                        ,udef-type
                                        null)
                     :initial-element nil))
       ;;
       ,(when clear-table-fn-sym
          `(defun ,clear-table-fn-sym ()
             (fill ,table-sym nil)))
       ;;
       (defun ,lookup (input idx end)
         (labels
             ((look (where)
                (cond
                  ((null where)
                   nil)
                  ((typep where ',udef-type)
                   (let ((have (,key where)))
                     (when (,test have input)
                       ;; found it!
                       where)))
                  ;; Check for termination before looping
                  ((eq where end)
                   nil)
                  ;; Loop over lists
                  ((consp where)
                   ;; Any list element could be a vector,
                   ;; so just recurse generically
                   (or (look (car where))
                       (look (cdr where))))
                  ;; Loop over arrays
                  ((typep where '(array ,udef-type (*)))
                   (some #'look where))
                  ;; Invalid data
                  (t
                   (error "unsupported element in dedup data: ~s" where)))))
           (look (svref ,table-sym idx))))
       ;;
       ,(when retrieve-fn-sym
          `(defun ,retrieve-fn-sym (input)
             (,lookup input
                      (udef-lookup-index input ,size nil)
                      :check-all)))
       ;;
       (defun ,save-fn-sym (input create-lambda &key int-hash-value)
         "Searches for INPUT, optionally using INT-HASH-VALUE;
         CREATE-LAMBDA is called to get a udef to insert only when necessary.
         The second value is T if INPUT was already found and NIL if it was inserted."
         (let* ((idx (udef-lookup-index input ,size int-hash-value))
                (end :first-run-checks-all))
           (labels
               ((insert ()
                  (let ((udef (funcall create-lambda)))
                    ;; TODO: optimize for only one element,
                    ;; store directly without cons cell?
                    ;; TODO: do CAS against the old START directly
                    ;; and only on failure loop again?
                    (sb-vm:without-arena
                      (sb-ext:atomic-push udef
                                          (svref ,table-sym idx)))
                    udef)))
             (let ((start (svref ,table-sym idx)))
               ;; We loop over the search --
               ;; in case items got added while the current thread
               ;; was looking. Yeah, there's still some chance
               ;; of duplicated data.
               (tagbody
                 :search
                 (let ((found (,lookup input idx end)))
                   (when found
                     (return-from ,save-fn-sym (values found t))))
                 ;; Not found.
                 ;; If no concurrent modifications...
                 (let ((now (svref ,table-sym idx)))
                   (when (eq start now)
                     (return-from ,save-fn-sym (values (insert) nil)))
                   ;; only walk until the previous starting point --
                   ;; we add only in front, so the later stuff
                   ;; has been checked already.
                   (setf end start
                         start now)
                   (go :search))))))))))
