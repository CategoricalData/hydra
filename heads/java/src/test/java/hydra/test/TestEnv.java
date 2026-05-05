// Hand-written test environment for Java.
// Provides a real graph with primitives and kernel bindings, mirroring Haskell's TestEnv.hs.
// Referenced directly by the generated TestGraph.java (no post-generation patching required).

package hydra.test;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.graph.Graph;
import hydra.context.Context;
import hydra.TestSuiteRunner;

import java.util.Map;

public class TestEnv {
    private static Graph cachedGraph;
    private static Context cachedContext;

    /**
     * Returns the test graph. The testTypes and testTerms arguments are
     * accepted for signature parity with the DSL declaration
     * (Map Name Type -> Map Name Term -> Graph); the actual graph is built
     * from the TestSuiteRunner and ignores both arguments — primitives and
     * kernel bindings are host-language specific and can't be expressed at
     * the DSL level.
     */
    public static Graph testGraph(Map<Name, Type> testTypes, Map<Name, Term> testTerms) {
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
