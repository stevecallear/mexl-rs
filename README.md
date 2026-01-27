# mexl

A simple expression language.

## Rules
* Any infix operation should attempt to unify the operands before execution. Null values should result in the default of the other type. Mismatched integer/float expressions should be unified to float/float. All other types should be left unchanged.
* Integer to float conversion should be specified by the user. Integer algebraic operations should result in an integer output (e.g. '3 / 2 = 1'). The user can cast one of the values to float to enforce a float output (e.g. '3 / 2 as float = 1.5'). A cast of an integer result will not affect the algebra (e.g. '(3 / 2) as float = 1.0').
* Null values should be coalesced in all scenarios, no runtime error should be returned due to the absence of a value.
* A builtin should have a guaranteed return type (e.g. integer for 'len'). It should handle null input, but should not be responsible for casting. If an invalid non-null input type is provided it should result in a runtime error (the user can simply cast before if needed).

## Todos
[ ] Error return if multiple expressions are specified (currently terminates ahead of EOF)
[ ] Index operator (strings, arrays)

## Ideas
[ ] Array member (e.g. x.0 == x[0], y.1 == y[1])
[ ] Slice operator (strings, arrays) e.g. [4..], [..3], [1..5]