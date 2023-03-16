package hydra;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.compute.Kv;
import hydra.core.Term;
import hydra.graph.Graph;
import hydra.test.testSuite.TestSuite;
import hydra.testing.TestCase;
import hydra.testing.TestGroup;
import hydra.tools.PrettyPrinter;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static hydra.Flows.*;
import static org.junit.jupiter.api.Assertions.*;


/**
 * Java executor for Hydra's language-agnostic test suite
 */
public class TestSuiteRunner extends HydraTestBase {
    @ParameterizedTest
    @MethodSource("provideTestCases")
    void runTestCase(String name, Term<Kv> input, Term<Kv> output) {
        Graph<Kv> graph = emptyGraph();
        String suffix = " (" + name + ")";

        Flow<Graph<Kv>, Term<Kv>> reduced = reduce(input);
        FlowState<Graph<Kv>, Term<Kv>> result = reduced.value.apply(graph).apply(EMPTY_TRACE);
        if (result.value.isPresent()) {
            if (!result.value.get().equals(output)) {
                // First, assert that the pretty-printed strings for the results are the same;
                // this provides more readable failure messages.
                assertEquals(print(output), print(result.value.get()),
                    "Original term does not reduce to expected term" + suffix);
                // Now check that the terms are truly equal
                assertEquals(output, result.value.get(), "Original term does not reduce to expected term" + suffix);
            }
        } else if (result.trace.messages.isEmpty()){
            Assertions.fail("Reduction failed" + suffix);
        } else {
            Assertions.fail("Reduction failed: " + result.trace.messages.get(0) + suffix);
        }
    }

    private static <A> String print(Term<A> term) {
        return PrettyPrinter.printTerm(term);
    }

    private static List<Arguments> provideTestCases() {
        List<Arguments> args = new ArrayList<>();
        addTestGroup(TestSuite.allTests, null, args);
        return args;
    }

    private static <A> void addTestCase(TestCase<A> testCase, int idx, String prefix, List<Arguments> args) {
        args.add(Arguments.of(testCaseDescription(testCase, prefix, idx), testCase.input, testCase.output));
    }

    private static <A> void addTestGroup(TestGroup<A> testGroup, String prefix, List<Arguments> args) {
        String newPrefix = testGroupDescription(testGroup, prefix);
        int idx = 0;
        for (TestCase<A> testCase : testGroup.cases) {
            addTestCase(testCase, ++idx, newPrefix, args);
        }
        for (TestGroup<A> subgroup : testGroup.subgroups) {
            addTestGroup(subgroup, newPrefix, args);
        }
    }

    private static <A> String testCaseDescription(TestCase<A> testCase, String prefix, int idx) {
        String name = testCase.description.orElse("test #" + idx);
        return prefix == null ? name : prefix + " > " + name;
    }

    private static <A> String testGroupDescription(TestGroup<A> testGroup, String prefix) {
        String name = testGroup.name + testGroup.description.map(s -> " (" + s + ")").orElse("");
        return prefix == null ? name : prefix + " > " + name;
    }
}
