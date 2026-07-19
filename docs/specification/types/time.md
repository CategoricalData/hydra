<!-- NOTE: this page will be automatically generated from the kernel type definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types/Time.hs (generator not
     yet built). Hand-authored draft under #417.
     The types/ pages document kernel types as needed by the primitive specifications;
     coverage is driven by those needs, not completeness. -->

# Time types

Kernel types from the `hydra.time` namespace, as used by the primitive specifications
(notably [FileStatus](files.md#filestatus) and `hydra.lib.system.getTime`).

## Timespec

An instant in time, with the semantics of the POSIX `struct timespec`: a number of seconds and
nanoseconds since the Unix Epoch (1970-01-01T00:00:00Z).
The actual resolution is implementation- and filesystem-defined.
A record with the following fields:

| Field | Type | Meaning |
|---|---|---|
| `seconds` | `int64` | whole seconds since the Unix Epoch; signed, so instants in the far past or distant future are representable |
| `nanoseconds` | `uint32` | nanoseconds within the second, in the range [0, 999999999]; unsigned, as the value is never negative |
