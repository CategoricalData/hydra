package hydra;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.compute.Kv;
import hydra.core.Term;
import hydra.graph.Graph;
import hydra.test.testSuite.TestSuite;
import hydra.testing.TestCase;
import hydra.testing.TestGroup;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static hydra.Flows.*;
import static org.junit.jupiter.api.Assertions.*;


/**
 * Java executor for Hydra's language-agnostic test suite
 */
public class TestSuiteRunner extends HydraTestBase {
    @Test
    public void runAllTestCases() {
        TestGroup<Kv> allTests = TestSuite.allTests;
        runTestGroup(emptyGraph(), allTests, 0);
    }

    private static <A> void runTestCase(Graph<A> graph, TestCase<A> testCase, int depth, int idx) {
        System.out.println(testCaseDescription(testCase, depth, idx));

        Flow<Graph<A>, Term<A>> reduced = reduce(testCase.input);
        FlowState<Graph<A>, Term<A>> result = reduced.value.apply(graph).apply(EMPTY_TRACE);
        if (result.value.isPresent()) {
            assertEquals(testCase.output, result.value.get(), "Original term does not reduce to expected term");
        } else if (result.trace.messages.isEmpty()){
            Assertions.fail("Reduction failed");
        } else {
            Assertions.fail("Reduction failed: " + result.trace.messages.get(0));
        }
    }

    private static <A> void runTestGroup(Graph<A> graph, TestGroup<A> testGroup, int depth) {
        System.out.println(testGroupDescription(testGroup, depth));

        int idx = 0;
        for (TestCase<A> testCase : testGroup.cases) {
            runTestCase(graph, testCase, depth + 1, ++idx);
        }

        for (TestGroup<A> subgroup : testGroup.subgroups) {
            runTestGroup(graph, subgroup, depth + 1);
        }
    }

    private static <A> String testCaseDescription(TestCase<A> testCase, int depth, int idx) {
        return indent(depth) + testCase.description.orElse("test #" + idx);
    }

    private static <A> String testGroupDescription(TestGroup<A> testGroup, int depth) {
        return indent(depth) + testGroup.name + testGroup.description.map(s -> " (" + s + ")").orElse("");
    }

    private static String indent(int depth) {
        return "    ".repeat(Math.max(0, depth));
    }
}
