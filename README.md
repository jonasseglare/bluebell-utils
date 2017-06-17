# bluebell-utils

A Clojure library designed to ... well, that part is up to you.

## Usage

FIXME

## bluebell.typed.core

Ett enda objekt: vexpress
Motsvarar ett abstrakt vektor-uttryck
Följande metoder:

 * dimension: (dimension [vexpress] ...) 
      returnerar dess dimension
 * expand: (expand [express cb] ...)
      genererar kod

Utilties:
 (to-array expr)
 (to-vec expr)
 (foreach 'i (fn [x ]

Funktioner som returnerar uttryck:
    (input-coll [dim] [expr]): 
      Tar en collection som input

    (add-vec [a] [b]):
      Returnerar ett uttryck som motsvarar add addera de två vektorerna

## bluebell.utils.access

  * Simplifies working with *informal domain types*, such as a "person" represented as a map.
  * Specifies in one place how a value in a composite type can be read, written, updated, etc.
  * Composable: Chain many accessors
  * Type correctness checking: An accessor encodes on (i) what composite object it is applied and (ii) the type of the object that we are accessing.
  * Brevity: All information in one place about how to access it...

**Type checking**: If we use access for all our objects, even small temporary ones returned from functions, we will catch many of the bugs we would otherwise catch with a static type system, because if we access every composite object in a systematic way using the accessors, we will get an error early on as soon as we attempt to read/write to it. Note, however, that it is in the accessors that all typing is done: We don't specify functions, let-values, etc.

### Extraction-type accessors
  - index-accessor
  - key-accessor
### Composite accessors
  - sequential
  - vector
  - map
### Transforming accessors
  - Optional: Turns value into optional
  - Forward/Backward (inc/dec) 

### Function constructs
  - Transformer (accessor and function)
  - Optional transformer
  - Poly fun [original-arglist extracted-var arglist-accessor]

### Async facility:
  - Async(f, [a, b, c, [c]])

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
