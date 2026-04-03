// Hand-written test environment for Java.
// Provides a real graph with primitives and kernel bindings, mirroring Haskell's TestEnv.hs.
// Referenced by the generated TestGraph.java after post-generation patching.

package hydra;

import hydra.graph.Graph;
import hydra.context.Context;

public class TestEnv {
    private static Graph cachedGraph;
    private static Context cachedContext;

    public static synchronized Graph testGraph() {
        if (cachedGraph == null) {
            cachedGraph = TestSuiteRunner.buildTestGraph();
        }
        return cachedGraph;
    }

    public static synchronized Context testContext() {
        if (cachedContext == null) {
            cachedContext = new Context(
                hydra.util.ConsList.empty(),
                hydra.util.ConsList.empty(),
                hydra.util.PersistentMap.empty());
        }
        return cachedContext;
    }
}
