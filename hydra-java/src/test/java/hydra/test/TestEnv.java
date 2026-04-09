// Hand-written test environment for Java.
// Provides a real graph with primitives and kernel bindings, mirroring Haskell's TestEnv.hs.
// Referenced by the generated test data after post-generation patching.

package hydra.test;

import hydra.graph.Graph;
import hydra.context.Context;
import hydra.TestSuiteRunner;

public class TestEnv {
    // Public fields referenced by generated TestGraph.java
    public static final Graph testGraph = buildTestGraph();
    public static final Context testContext = buildTestContext();

    private static Graph cachedGraph;
    private static Context cachedContext;

    private static Graph buildTestGraph() {
        return TestSuiteRunner.buildTestGraph();
    }

    private static Context buildTestContext() {
        return new Context(
            hydra.util.ConsList.empty(),
            hydra.util.ConsList.empty(),
            hydra.util.PersistentMap.empty());
    }

    public static Graph testGraph() {
        if (cachedGraph == null) {
            cachedGraph = TestSuiteRunner.buildTestGraph();
        }
        return cachedGraph;
    }

    public static Context testContext() {
        if (cachedContext == null) {
            cachedContext = new Context(
                hydra.util.ConsList.empty(),
                hydra.util.ConsList.empty(),
                hydra.util.PersistentMap.empty());
        }
        return cachedContext;
    }
}
