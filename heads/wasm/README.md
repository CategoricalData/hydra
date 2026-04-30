# Wasm head

Runtime infrastructure for executing Hydra kernel modules compiled to
WebAssembly.

The Wasm target compiles Hydra DSL modules to WAT via the coder in
`packages/hydra-wasm/`. This directory provides the other half: a harness
that loads those modules into a Wasm runtime, resolves their imports,
invokes their exports, and runs the kernel test suite against them.

See [feature_325_wasm-plan.md](../../feature_325_wasm-plan.md) for the
overall milestone plan.

## Layout

```
heads/wasm/
├── README.md               # this file
├── host-abi.md             # contract between Wasm modules and host runtimes
├── bin/
│   └── test-wasm.sh        # entry point to run the test suite
├── runtimes/
│   └── node/
│       └── harness.js      # Node.js runtime adapter (default)
└── m1-manifest.json        # M1 smoke test manifest (temporary, per-milestone)
```

## Runtimes

The default runtime is **Node.js** (>= 20). It uses only the built-in
`WebAssembly` global — no npm dependencies.

A second runtime (wasmtime) is planned; it will be added under
`runtimes/wasmtime/` once the host-import surface stabilizes. See
[host-abi.md](host-abi.md) for the portable contract both runtimes honor.

### Prerequisites

- **Node.js** `>= 20` (for `WebAssembly` global and ES modules).
- **wabt** (`wat2wasm`) — WAT is compiled to binary Wasm at runtime. Install
  via `brew install wabt` or the distro package of the same name.

## Running tests

```bash
# M1 smoke test (hydra.constants.* with no-arg exports)
heads/wasm/bin/test-wasm.sh

# A custom manifest
heads/wasm/bin/test-wasm.sh path/to/manifest.json
```

## Manifest format (M1)

```json
{
  "wasm_file": "dist/wasm/hydra-kernel/src/main/wat/hydra/constants.wat",
  "tests": [
    { "name": "...", "export": "hydra.constants.max_int32",
      "kind": "i32", "expected": 2147483647 },
    { "name": "...", "export": "hydra.constants.keyClasses",
      "kind": "string", "expected": "classes" }
  ]
}
```

Fields:

- `wasm_file` — path to the `.wat`, relative to the worktree root.
- `tests[].export` — Wasm function name to invoke (zero-arg).
- `tests[].kind` — how to decode the result: `"i32"` or `"string"`.
- `tests[].expected` — expected decoded value.

This schema is temporary and milestone-specific; later milestones will
replace it with coder-generated manifests and then (eventually) with direct
traversal of `hydra.test.TestSuite` from Wasm memory.

## Milestones

See `feature_325_wasm-plan.md`. In brief:

- **M1** (here): harness + manifest + a handful of zero-arg constants run.
- **M2**: ~20 first-order kernel functions run, first primitive library
  implementations added.
- **M3**: simple `hydra.test.lib.*` tests run.
- **M4**: closures (the wall).
- **M5**: remaining primitives, polymorphism details.
- **M6**: full kernel test suite passes.
