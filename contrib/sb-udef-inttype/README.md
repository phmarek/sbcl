# User-Defined Integer-Types


A tag (`UDEF-INTTYPE-LOWTAG`) is reserved for user-defined integers;
another 8 bits (`+UDEF-TAG-BITS+`) provide several different types.

That means that besides the standard Common Lisp numeric tower
up to 256 different integer types with (on 64bit architectures) 48 bits each
can be defined, and are held as distinct types even during runtime.

This makes UDEFs` "better" than reusing `NaN` values,
reserving `FIXNUM`s ranges, and so on.


# Separate Numeric Types

One use case is simply providing non-standard numeric types -
eg. a probability value.


```lisp
(defconstant +prob-bits+ 16)

(sb-udef-inttype:def-udef-inttype probability
                                  :max-bits +prob-bits+
                                  :constructor make-probability%
                                  :reader get-probability-value%)

(defun make-probability (x)
  "Converts a value 0.0 to 1.0 to a probability"
  (declare (type (float 0.0 1.0) x))
  (make-probability% (* x
                        (1- (ash 1 +prob-bits+)))))

(defun get-probability-value (x)
  "Returns the 0.0 to 1.0 value of a probability"
  (/ (get-probability-value% x)
     (ash 1 +prob-bits+)))
```


# A Column-Based Structure

Another nice use-case is using distinct UDEFs as indices in (large) arrays;
this way one UDEF value is comparable to a (pointer to a) class instance,
as it allows to address multiple slots via multiple arrays - 
that's what the `DEF-COLUMN-STRUCT` macro is about.

The big advantage is that a few _specialized_ arrays with ten million rows
are much faster for GC than ten million structures or CLOS class instances;
see `s-l-a-d-comparison.lisp` (and the `Makefile`).

Note that this only works for basic types like `(unsigned-byte 8)`;
using a composite type (like `STRING`) in a slot means
that the slot itself only stores a pointer to the actual `STRING` instance,
which gets you millions of `STRING` instances again. \
To avoid that, using constant-sized vectors as slot type will allocate
correspondingly larger storage vectors; `WITH-C-S-SLOTS` then uses
displaced arrays. \
For dynamically-sized data, see /Buffer indexing/ below.


```lisp
(sb-column-struct:def-column-struct (person
                                      (:max-bits 16)
                                      (:base-constructor make-person)
                                      (:initial-size 50000))
  (name   nil :type symbol)
  (id       0 :type (unsigned-byte 32))
  (mother nil :type person)
  (uuid   nil :type (array (unsigned-byte 8) 32)))


(defvar *somebody* (make-person :name 'Miller 
                                :id 42
                                :uuid #(1 2 3 4 5 6 7 8 ...))

(person-name *somebody*)
```

Note that accessing a flattened vector (like the `UUID` above) means 
referencing it via an displaced array; perhaps optionally providing
a copy (via SUBSEQ) might be helpful, though
this makes a difference for GC and when SETFing individual elements.

The basic premise is to use C-S as a memory-efficient read-only database.



There are two main modes of using a column-struct:
- with one intermediate level, ie. a slot vector directly addressing 
  large storage vectors,
- with two levels, the slot vector pointing to batches of storage vectors.

The first case is faster and easier for fixed-size allocations;
if reallocation becomes necessary (eg. because more elements are used than
initially allocated), concurrent write accesses might get lost
while pre-existing elements are copied into the newly allocated storage vector.

The second one allows thread-safe enlargements, by allocating
additional storage vectors and installing them via new batch vectors;
this way concurrent writes to existing elements are always safe.
Also, with a typical batch size of eg. 1 million elements,
the batch vectors are quite small, so any extension only needs
to copy a few elements. \
(When passing `T` as `:BATCH-SIZE`, `DEFAULT-BATCH-SIZE` runs
a heuristic to provide a "sane" value.)

Diagram:
```
1 level, for $n$ elements

  ┌slots┐
  │  0  │──────────────>┌─slot-a─┐
  │  1  │──>┌─slot-b─┐  │  a[0]  │
  │ ... │   │  b[0]  │  │  a[1]  │
  └─────┘   │  b[1]  │  │  ...   │
            │  ...   │  │ a[n-1] │
            │ b[n-1] │  └────────┘
            └────────┘

2 levels, for $m$ batches of $n$ elements each, totaling $n*m$

  ┌slots┐
  │  0  │──>┌──batch──┐
  │  1  │   │ [ 0- n] │─>┌─slot-a─┐
  │ ... │   │ [ n-2n] │  │  a[0]  │
  └─────┘   │ [2n-3n] │  │  a[1]  │
            │ [ ... ] │  │  ...   │
            │ [...mn] │  │ a[n-1] │
            └─────────┘  └────────┘
```

When a `UDEF` is given as slot type, the storage vectors only allocate
a corresponding `UNSIGNED-BYTE` type, and translation (ie. boxing) is done
when accessing elements. \
Also, the all-one value for that `UNSIGNED-BYTE` is taken to mean `NIL`;
so in the `PERSON` example above, a `:mother NIL` gets converted to/from `#xffff`.


# Bitfields

With 48 usable bits, more than one data item can be stored in a `UDEF`.

This is what `DEF-BITFIELD-STRUCT` provides: an immediate values consisting of multiple parts.


```lisp
(sb-udef-inttype:def-bitfield-struct bbbits
  (eight 255 :type (unsigned-byte 8) :modulo t)
  (four . 4)
  (one 1 :type (unsigned-byte 1)))


(defparameter *x* 
  (make-bbbits :eight #x55 :four #xa :one 0))

(bbbits-eight *x*)

(setf (bbbits-four *x*)
      2)
```

This is used in /Buffer indexing/, see below.


# Buffer-Indexing

When storing lots of variable-length data, allocating many `STRING`s or
`(UNSIGNED-BYTE 8)` vectors mean high GC load again.

`MAKE-UDEF-ADDRESSED-BUFFER` creates a `UDEF` immediate that stores
the length and index in large storage vectors together;
eg. strings with a maximum length of 255 and totaling up to $2^24$ characters
can be packed into a 32bit value.

```lisp
(MAKE-UDEF-ADDRESSED-BUFFER filename
  :len-bits 8
  :index-bits 24)
```

When using this type in a /column-struct/, the storage vectors
will be allocated as `(UNSIGNED-BYTE 32)`, 
meaning compact storage and no work during GC. \
With more than 16M of `filename`s, more `:INDEX-BITS` become necessary;
then the storage vector will need to use an element-type `(UNSIGNED-BYTE 64)`,
which is not quite as compact, but still reduces GC load, compared to
some 100'000 or millions of `STRING` instances.


-----------------------------------
- Var-len Data gets copied in and out (SUBSEQ), so no modification possible later on


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

