# Host ABI for Hydra Wasm modules

This file is the contract between Hydra's generated Wasm modules and any host
runtime (Node, wasmtime, etc.). All conforming runtimes implement the same set
of imported functions with matching semantics.

Keeping both runtimes honoring this contract is how we make the Wasm target
runtime-agnostic. Changes here require updates to every runtime adapter under
`heads/wasm/runtimes/`.

## Calling conventions

- All parameters and results are `i32` unless explicitly noted.
- Strings are passed as a pointer into the module's linear memory. The layout
  is `[len:i32 little-endian][UTF-8 bytes...]`. The caller owns the buffer;
  the callee must copy any data it intends to retain past the call.
- Compound values (records, unions, lists, maps, etc.) are passed as `i32`
  pointers into linear memory. Layout is defined by the Hydra Wasm coder
  (`packages/hydra-wasm/src/main/haskell/Hydra/Sources/Wasm/Coder.hs`).
- Return values that do not fit in `i32` are written through an out-parameter
  pointer provided by the caller.

## Wasm-side features in use

- Wasm MVP (1.0) baseline.
- `funcref` + `call_indirect` (for closures, starting at M4).

Runtime-specific features (GC proposal, exceptions, multi-value, reference
types beyond funcref, threads) are **not** used. Any future use requires
explicit design review — the goal is portability across every standalone Wasm
runtime and the browser.

## Imports

Each generated Wasm module imports zero or more host functions. The full set
is the union of what each module uses. Runtime adapters implement all of them
so they can load any kernel module.

### M1

(No imports yet. `hydra.constants.*` uses only data segments and returns
constants or pointers into the segment.)

### M2 (planned)

TBD. Likely candidates:

| Import | Signature | Semantics |
|---|---|---|
| `env.abort(code, msg_ptr, msg_len)` | `(i32, i32, i32) -> ()` | Terminate execution with a diagnostic message. Harness prints the string and marks the currently-running test as failed. |
| `env.log(msg_ptr, msg_len)` | `(i32, i32) -> ()` | Print a UTF-8 string. Non-fatal; used for trace output. |

These will be added as the corresponding kernel functionality starts needing
them. This table is authoritative — no runtime is allowed to provide imports
not listed here, and no Wasm module is allowed to import names not listed here.

## Exports

Generated Wasm modules export:

- `memory` — the module's linear memory, as a Wasm memory instance.
- `__alloc(sz: i32) -> i32` — bump allocator. Returns a pointer to `sz` bytes
  of fresh linear memory. Memory is not reclaimed (no GC in the baseline).
- `__bump_ptr` — the bump allocator's next-free pointer, as a mutable global.
- One function per top-level Hydra term definition in the module. Function
  names match the Hydra namespace: `hydra.foo.bar.baz` in the DSL becomes
  export `"hydra.foo.bar.baz"` in Wasm, with body name `$hydra.foo.bar.baz`
  (snake-cased local part).

## Versioning

This document versions alongside the Hydra Wasm coder. When the coder changes
the emitted layout or export conventions in a breaking way, this file is
updated in the same commit.
