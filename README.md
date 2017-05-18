# epicea-utils

A Clojure library designed to ... well, that part is up to you.

## Usage

FIXME

## epicea.utils.access

  * Simplifies working with *informal domain types*, such as a "person" represented as a map.
  * Specifies in one place how a value in a composite type can be read, written, updated, etc.
  * Composable: Chain many accessors
  * Type correctness checking: An accessor encodes on (i) what composite object it is applied and (ii) the type of the object that we are accessing.
  * Brevity: All information in one place about how to access it...

**Type checking**: If we use access for all our objects, even small temporary ones returned from functions, we will catch many of the bugs we would otherwise catch with a static type system, because if we access every composite object in a systematic way using the accessors, we will get an error early on as soon as we attempt to read/write to it. Note, however, that it is in the accessors that all typing is done: We don't specify functions, let-values, etc.

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
