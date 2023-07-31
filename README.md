# OCaml Examples
List of examples demonstrating various language features.

## Effects
The following examples are dedicated to experiments with algebraic effects and
handlers:

* [lib/state.ml](lib/state.ml) gives an example of a stateful computations
using effects. The computation is itself pure, and the stateful computation is
within the algebraic effects. The implementation uses stateful function similar
to a state monad. The advantage is however that the computation is direct,
without the need for everything to be within a monad.

* [lib/env.ml](lib/env.ml) tries to show two things
  1. How we can use OCaml modules as type classes, similar to those in Haskell.
  2. Using type classes we can construct a polymorphic reader effects. That is
     computations that can read from a shared environment.
	 
## Type classes
Although OCaml does not directly provide type classes, its powerful module
system means we can implement type classes in terms of "module values".
[lib/classes.ml)(lib/classes.ml) provides the module specification for a natural
element and a monoid. Then it goes on to use this to make `'a list monoid`. An
example of how the `'a list monoid` module value can be used to make type
constraints is demonstrated in [lib/env.ml](lib/env.ml).
 
