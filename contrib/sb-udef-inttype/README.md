# User-Defined Integer-Types

SBCL allocates a tag (+UDEF-RESERVED-LOW-BITS+) for user-defined integers;
another indirection uses another 8 bits to provide several different types.

That means that besides the standard Common Lisp numeric tower
up to 256 different integer types with (on 64bit architectures) 48 bits each
can be defined, and are held as distinct types even during runtime[1].



Ad 1: This differs from stuff like reusing NaN values,
simple FIXNUMs, and so on.


## Use-Case 1: Separate Numeric Types

One use case is providing non-standard numeric types -
eg. a probability value.


```lisp
(defconstant +prob-bits+ 16)

(sb-impl::def-udef-inttype probability
                           :max-bits +prob-bits+
                           :constructor make-probability%
                           :reader get-probability-value%)

(defun make-probability (x)
  "Converts a value 0.0 to 1.0 to a probability"
  (make-probability% (* x
                        (ash 1 +prob-bits+))))

(defun get-probability-value (x)
  "Returns the 0.0 to 1.0 value of a probability"
  (/ (get-probability-value% x)
     (ash 1 +prob-bits+)))
```


## Use-Case 2: a Column-Based Structure

Another nice use-case is using distinct UDEFs as indizes in (large) arrays;
this way one UDEF value is comparable to a class instance,
as it allows to address multiple values in multiple arrays - 
that's what the SB-COLUMN-STRUCT contrib is about.

A big advantage is that a few specialized arrays with ten million rows
are much faster on GC than ten million structures or CLOS class instances --
See contrib/sb-udef-inttype/s-l-a-d-comparison.lisp (and the Makefile).


```lisp
(sb-column-struct:def-column-struct (person
                                      (:base-constructor make-person)
                                      (:initial-size 50000))
  (name nil :type symbol)
  (id     0 :type (unsigned-byte 32)))


(defvar *somebody* (make-person :name 'Miller :id 42))

(person-name *somebody*)
```

See contrib/sb-udef-inttype/column-structure.lisp for more information.


## Example 2:

For really small structures (up to 48 bits actual data) the
SB-BITFIELD-STRUCT might be helpful.


```lisp
(sb-bitfield-struct:def-bitfield-struct bb-bits
  (eight 255 :type (unsigned-byte 8))
  (two 3 :type (unsigned-byte 2)))
  (one 1 :type (unsigned-byte 1)))

(defparameter *bbmax* (make-bbbits))
(defparameter *bbzero* (make-bbbits :eight 0 :two 0 :one 0))
```
