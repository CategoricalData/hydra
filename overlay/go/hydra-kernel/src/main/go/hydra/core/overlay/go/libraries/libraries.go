// Package libraries is the primitive registry for the Go head: it will expose
// the standard hydra.lib.* primitives as a list of registrable
// PrimitiveFunction values, keyed by their canonical names (derived from the
// generated PrimitiveDefinition data, never hand-maintained), for the reducer
// and the test-suite runner.
//
// It is intentionally minimal at this stage. The Go head currently runs as a
// code-generation TARGET: the generated kernel imports the native lib packages
// (hydra/overlay/go/lib/*) directly, so no runtime registry is needed to
// compile or execute generated code. The registry is required only by the
// kernel test-suite runner (issue #289, the test-runner milestone), which will
// populate Standard from the generated def-modules and the native
// implementations.
package libraries

// Standard returns the standard primitive set. It is a placeholder pending the
// test-runner milestone, which wires each hydra.lib.* primitive's generated
// definition to its native overlay implementation with the correct arity,
// laziness, and purity metadata.
func Standard() []any { return nil }
