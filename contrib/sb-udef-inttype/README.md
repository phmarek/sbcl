# User-Defined Integer-Types

A machine word consists of eg. 64 bits:

  +--------------------------------------------------------------+
  |MSB                                                        LSB|
  +--------------------------------------------------------------+
  64                                                             0

SBCL uses a few bits to tag values (FIXNUM, pointers, CONSes, etc.);
one of the available tags is now reserved (`UDEF-INTTYPE-LOWTAG`):

  +-------------------------------------------------------+------+
  |MSB                                                    |lowtag|
  +-------------------------------------------------------+------+
  64                                                     8|7     0

On top of that, this contrib uses further 8 bits (`+UDEF-TAG-BITS+`)
to provide several different types of integers.

  +-----------------------------------------------+-------+------+
  |MSB                                            | udef  |lowtag|
  +-----------------------------------------------+-------+------+
  64                                            16|15    8|7     0

That means that besides the standard Common Lisp numeric tower
up to 256 different integer types with (on 64bit architectures) 48 bits each
can be defined, and are held as distinct types even during runtime.

This makes UDEFs` "better" than reusing `NaN` values,
reserving `FIXNUM`s ranges, and so on.


# Use-Cases

## Separate Numeric Types

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


## A Column-Based Structure

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
(def-column-struct (person
                    (:max-bits 16)
                    (:constructor make-person)
                    (:initial-size 50000))
  (name   nil :type symbol)
  (id       0 :type (unsigned-byte 32))
  (mother nil :type person)
  (uuid   nil :type (array (unsigned-byte 8) 32)))


(defvar *somebody* (make-person :name 'Miller 
                                :id 42
                                :uuid #(1 2 3 4 5 6 7 8 ...))

(person-name *somebody*)

(with-c-s-slots (person *somebody*) (name (person-id id))
  (format t "~s ~d~%" name person-id))
```

Note that accessing a flattened vector (like the `UUID` above) means 
referencing it via an displaced array; perhaps optionally providing
a copy (via SUBSEQ) might be helpful,
although this makes a difference for GC and would
inhibit SETFing individual elements.

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


## Bitfields

With 48 usable bits, more than one data item can be stored in a `UDEF`.

This is what `:allocation :immediate` provides: a machine word consisting of multiple parts.


```lisp
(sb-udef-inttype:def-bitfield-struct bbbits
  (eight 255 :type (unsigned-byte 8) :allocation :immediate)
  (four    0 :type (unsigned-byte 4) :allocation :immediate)
  (one     0 :type (unsigned-byte 1) :allocation :immediate))

(defparameter *x* 
  (make-bbbits :eight #x55 :four #xa :one 0))

(bbbits-eight *x*)

(setf (bbbits-four *x*)
      2)
```

This is used in /Buffer indexing/, see below.

With only immediate slots no storage vectors are needed.


## Buffer-Indexing

When storing lots of variable-length data, allocating many `STRING`s or
`(UNSIGNED-BYTE 8)` vectors mean high GC load again.

Defining a column-structure that stores the length and an index into a large storage vector,
only a few specialized arrays get allocated -- so GC load is minimal.

Eg. strings with a maximum length of 255 and totaling up to $2^24$ characters
can be packed into a 32bit value;
by using the correct slot order and a default value form the length doesn't even have to be manually specified.

```lisp
(def-column-struct (filename
                    (:index-bits 24)
                    (:max-bits   32)
                    (:batch-size (* 1024 1024)))
 (name ""            :type (array character (len)))
 (len  (length name) :type (unsigned-byte 8)        :allocation :immediate))

(make-filename :name "/etc/services")
```


This provides a layout like this:

  +----------------+------+-----------------------+-------+------+
  |MSB             | len  |          index        | udef  |lowtag|
  +----------------+------+-----------------------+-------+------+
  64             48|47  40|39                   16|15    8|7     0

The `LEN` slot is stored in the UDEF-tagged machine word,
and the index points into large vectors of 1MB each.

# Other interesting points


## Storing UDEFs

When UDEFs are used as slot types in a /column-struct/, the underlying storage vectors
will be allocated with a matching `(UNSIGNED-BYTE x)` element-type, 
meaning compact storage and no work during GC.

As to the above example, with more than 16M of `filename`s, more `:INDEX-BITS` become necessary;
then the storage vector will need to use an element-type `(UNSIGNED-BYTE 64)`,
which is not quite as compact, but still reduces GC load, compared to
some 100'000 or millions of `STRING` instances. \
(Multiplying/shifting the indizes a bit to address a larger backend storage is not implemented yet.)
