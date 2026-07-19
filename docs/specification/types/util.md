<!-- NOTE: this page will be automatically generated from the kernel type definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types/Util.hs (generator not
     yet built). Hand-authored draft under #417.
     The types/ pages document kernel types as needed by the primitive specifications;
     coverage is driven by those needs, not completeness. -->

# Utility types

Kernel types from the `hydra.util` namespace, as used by the primitive specifications.

## Comparison

An ordering judgement: the result of comparing two values.
An enumeration with three values, in order:

| Value | Meaning |
|---|---|
| `lessThan` | the first value precedes the second |
| `equalTo` | the values are equal |
| `greaterThan` | the first value follows the second |

Returned by `hydra.lib.ordering.compare`; see
[ordering-and-equality.md](../ordering-and-equality.md) for the total order it judges against.
