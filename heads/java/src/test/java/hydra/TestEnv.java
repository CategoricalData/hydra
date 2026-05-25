// Hand-written test environment for the bootstrapping demo's Java target.
// Provides a real graph with primitives and kernel bindings.
// (The per-package hydra.test.TestEnv at heads/java/src/test/java/hydra/test/
// is the runtime counterpart of the DSL stub Hydra.Sources.Test.TestEnv;
// this file is kept under hydra.* for the bootstrapping demo's setup-java-target.sh.)

package hydra;

import hydra.graph.Graph;
import hydra.typing.InferenceContext;

public class TestEnv {
    private static Graph cachedGraph;
    private static InferenceContext cachedContext;

    public static synchronized Graph testGraph() {
        if (cachedGraph == null) {
            cachedGraph = TestSuiteRunner.buildTestGraph();
        }
        return cachedGraph;
    }

    public static synchronized InferenceContext testContext() {
        if (cachedContext == null) {
            cachedContext = new InferenceContext(0, new java.util.ArrayList<>());
        }
        return cachedContext;
    }
}
