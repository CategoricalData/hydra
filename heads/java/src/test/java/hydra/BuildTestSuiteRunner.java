package hydra;

import hydra.testing.TestGroup;
import hydra.test.build.TestSuite;

import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.TestFactory;

import java.util.stream.Stream;

/**
 * Java executor for hydra-build's own test suite. Package-scoped counterpart to
 * TestSuiteRunner (hydra-kernel); part of #547's per-package test aggregation. JUnit
 * discovers this class alongside TestSuiteRunner automatically, so no
 * composition/registration step is needed.
 *
 * All of hydra-build's test cases are universal (pure string comparison); no test-graph
 * construction is needed here (unlike TestSuiteRunner, hydra-build's tests don't evaluate
 * against a primitive-backed Graph). Uses the same generic walker (HydraTestGroupWalker)
 * as the kernel runner.
 */
public class BuildTestSuiteRunner {

    private static final String BENCHMARK_OUTPUT = System.getenv("HYDRA_BENCHMARK_OUTPUT");

    @TestFactory
    Stream<DynamicNode> buildTests() {
        TestGroup allTests = TestSuite.allTests();
        return new HydraTestGroupWalker("java", BENCHMARK_OUTPUT).walk(allTests, 0.0);
    }
}
