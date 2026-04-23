// Hand-written test environment for Scala.
// Provides a real graph with primitives and kernel bindings, mirroring Haskell's TestEnv.hs.
// Referenced by the generated test data after code generation.

package hydra.test.testEnv

// Signature parity with the DSL: testGraph accepts a Map[hydra.core.Name, hydra.core.Type]
// for parity with Haskell's `testGraph :: Map Name Type -> Graph`. The actual graph is
// built from the TestSuiteRunner and ignores the argument — primitives and kernel bindings
// are host-language specific and can't be expressed at the DSL level.
private var cachedGraph: hydra.graph.Graph = null

def testGraph(testTypes: Map[hydra.core.Name, hydra.core.Type]): hydra.graph.Graph = {
  if (cachedGraph == null) {
    cachedGraph = hydra.TestSuiteRunner.buildTestGraph()
  }
  cachedGraph
}

lazy val testContext: hydra.context.Context = hydra.context.Context(
  trace = Seq.empty,
  messages = Seq.empty,
  other = Map.empty)
