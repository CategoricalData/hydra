// Hand-written test environment for Scala.
// Provides a real graph with primitives and kernel bindings, mirroring Haskell's TestEnv.hs.
// Referenced by the generated test data after code generation.

package hydra.test.testEnv

lazy val testGraph: hydra.graph.Graph = hydra.TestSuiteRunner.buildTestGraph()

lazy val testContext: hydra.context.Context = hydra.context.Context(
  trace = Seq.empty,
  messages = Seq.empty,
  other = Map.empty)
