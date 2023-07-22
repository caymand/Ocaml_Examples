# OCaml Examples
List of examples demonstrating various language features.

## Effects
The following examples are dedicated to experiments with algebraic effects and
handlers:

** [lib/state.ml](lib/state.ml) gives an example of a stateful computations
using effects. The computation is itself pure, and the stateful computation is
within the algebraic effects. The implementation uses stateful function similar
to a state monad. The advantage is however that the computation is direct,
without the need for everything to be within a monad.
