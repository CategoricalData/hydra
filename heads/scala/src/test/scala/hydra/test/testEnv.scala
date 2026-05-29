// Hand-written test environment for Scala.
// Provides a real graph with primitives and kernel bindings, mirroring Haskell's TestEnv.hs.
// Referenced by the generated test data after code generation.

package hydra.test.testEnv

// Signature parity with the DSL: testGraph accepts a Map[Name, Type] and a
// Map[Name, Term] for parity with `testGraph :: Map Name Type -> Map Name Term -> Graph`.
// The actual graph is built from the TestSuiteRunner and ignores both arguments —
// primitives and kernel bindings are host-language specific and can't be expressed
// at the DSL level.
private var cachedGraph: hydra.graph.Graph = null

// Curried signature matches the Scala coder's emission pattern for
// multi-arg DSL functions: Map Name Type -> Map Name Term -> Graph
// becomes (testTypes)(testTerms) at the call site.
def testGraph(testTypes: Map[hydra.core.Name, hydra.core.Type])
             (testTerms: Map[hydra.core.Name, hydra.core.Term]): hydra.graph.Graph = {
  if (cachedGraph == null) {
    cachedGraph = hydra.TestSuiteRunner.buildTestGraph()
  }
  cachedGraph
}

lazy val testContext: hydra.typing.InferenceContext = hydra.typing.InferenceContext(
  freshTypeVariableCount = 0,
  trace = Seq.empty)
