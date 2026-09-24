// Hand-written test environment for Java.
// Provides a real graph with primitives and kernel bindings, mirroring Haskell's TestEnv.hs.
// Referenced directly by the generated TestGraph.java (no post-generation patching required).

package hydra.core.test;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.graph.Graph;
import hydra.core.typing.InferenceContext;
import hydra.TestSuiteRunner;

import java.util.Map;

public class TestEnv {
    private static Graph cachedGraph;
    private static InferenceContext cachedContext;

    /**
     * Returns the test graph. The testTypes and testTerms arguments are
     * accepted for signature parity with the DSL declaration
     * (Map Name Type -> Map Name Term -> Graph); the actual graph is built
     * from the TestSuiteRunner and ignores both arguments — primitives and
     * kernel bindings are host-language specific and can't be expressed at
     * the DSL level. Delegates to TestSuiteRunner.getTestGraph() so that
     * HYDRA_DEFAULT_IMPLS is honored (that cache is the USE_DEFAULT_IMPLS-aware
     * one; TestSuiteRunner.buildTestGraph() always builds a native-only graph).
     */
    public static Graph testGraph(Map<Name, Type> testTypes, Map<Name, Term> testTerms) {
        if (cachedGraph == null) {
            cachedGraph = TestSuiteRunner.getTestGraph();
        }
        return cachedGraph;
    }

    public static InferenceContext testContext() {
        if (cachedContext == null) {
            cachedContext = new InferenceContext(0, new java.util.ArrayList<>());
        }
        return cachedContext;
    }
}
