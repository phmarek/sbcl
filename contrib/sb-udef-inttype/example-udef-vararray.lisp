(in-package :col-struct-example)


(sb-udef-inttype:def-column-struct (person-name
                                     (:index-bits 24) ; store up to 2^24 characters
                                     (:max-bits 32) ; immediate data fits in ub-32 slots
                                     (:initial-size (* 128 1024)))
  (name (make-array 0 :element-type 'character :initial-element #\Nul)
       :type (array character (len)))
  (len (length name)
       :type (unsigned-byte 8)
       :allocation :immediate))


;; Forward declaration for siblings
(sb-impl::def-udef-inttype person
  :max-bits 16)

(sb-udef-inttype:def-column-struct (siblings
                                     (:index-bits 24) ; store up to 2^24 siblings
                                     (:max-bits 32) ; immediate data fits in ub-32 slots
                                     (:initial-size 16))
  ;; 31 siblings is enough for everyone
  (vec nil
       :type (array person (number-of-siblings)))
  (number-of-siblings (length vec)
                      :type (unsigned-byte 5)
                      :allocation :immediate))


(sb-udef-inttype:def-column-struct (person
                                     (:max-bits 16) ; store up to 2^16-1 persons
                                     (:batch-size (* 1024 4)))
  ;;
  (name     nil :type person-name)
  (age      0   :type (unsigned-byte 8))
  (mother   nil :type person)
  (father   nil :type person)
  (siblings nil :type siblings))

(let* ((m  (make-person :name (make-person-name :name   "mama") :age 35))
       (c1 (make-person :name (make-person-name :name "child1") :age 3 :mother m))
       (c2 (make-person :name (make-person-name :name "child2") :age 5 :mother m))
       (c3 (make-person :name (make-person-name :name "child3") :age 7 :mother m)))
  (setf (person-siblings c1)
        (make-siblings :vec (vector c2 c3)))
  (values m c1 c2 c3))
