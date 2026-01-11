package hydra;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Term;
import hydra.graph.Graph;
// import hydra.test.testSuite.TestSuite; // TODO: Restore when TestSuite is generated
import hydra.testing.TestCase;
import hydra.testing.TestCaseWithMetadata;
import hydra.testing.TestGroup;
import hydra.tools.PrettyPrinter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.List;

import static hydra.dsl.Flows.EMPTY_TRACE;
import static org.junit.jupiter.api.Assertions.assertEquals;


/**
 * Java executor for Hydra's language-agnostic test suite.
 */
@Disabled("Depends on Reduction.reduce() which is not yet available")
public class TestSuiteRunner extends HydraTestBase {

    @ParameterizedTest
    @MethodSource("provideTestCases")
    void runParameterizedTestCase(String name, Term input, Term output) {
        runReductionTestCase(true, name, input, output);
    }

    /**
     * Run a beta-reduction test case.
     */
    public static void runReductionTestCase(boolean eager, String name, Term input, Term output) {
        Graph graph = emptyGraph();
        String suffix = " (" + name + ")";

        // Flow<Graph, Term> reduced = Reduction.reduce(eager, input);
        // TODO: Uncomment when Reduction.reduce is available
        Flow<Graph, Term> reduced = null; // Placeholder

        // Skip test if Reduction.reduce is not yet implemented
        if (reduced == null) {
            Assertions.fail("Reduction.reduce() is not yet implemented (requires Strip and Extras modules)" + suffix);
            return;
        }

        FlowState<Graph, Term> result = reduced.value.apply(graph).apply(EMPTY_TRACE);
        if (result.value.isJust()) {
            if (!result.value.fromJust().equals(output)) {
                // First, assert that the pretty-printed strings for the results are the same;
                // this provides more readable failure messages.
                assertEquals(print(output), print(result.value.fromJust()),
                        "Original term does not reduce to expected term" + suffix);
                // Now check that the terms are truly equal
                assertEquals(output, result.value.fromJust(),
                        "Original term does not reduce to expected term" + suffix);
            }
        } else if (result.trace.messages.isEmpty()) {
            Assertions.fail("Reduction failed" + suffix);
        } else {
            Assertions.fail("Reduction failed: " + result.trace.messages.get(0) + suffix);
        }
    }

    private static String print(Term term) {
        return PrettyPrinter.printTerm(term);
    }

    private static List<Arguments> provideTestCases() {
        List<Arguments> args = new ArrayList<>();
        // addTestGroup(TestSuite.allTests(), null, args); // TODO: Uncomment when TestSuite is available
        return args;
    }

    private static void addTestCase(TestCaseWithMetadata testCase, int idx, String prefix, List<Arguments> args) {
        testCase.case_.accept(new TestCase.Visitor() {
            @Override
            public Object visit(TestCase.AlphaConversion instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.CaseConversion instance) {
                // TODO
                return null;
            }

            @Override
            public Object visit(TestCase.DeannotateTerm instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.DeannotateType instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.DelegatedEvaluation instance) {
                // TODO
                return null;
            }

            @Override
            public Object visit(TestCase.EtaExpansion instance) {
                // TODO
                return null;
            }

            @Override
            public Object visit(TestCase.FlattenLetTerms instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.FreeVariables instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.Evaluation instance) {
                args.add(Arguments.of(testCaseDescription(testCase, prefix, idx),
                        instance.value.input, instance.value.output));
                return null;
            }

            @Override
            public Object visit(TestCase.Inference instance) {
                // TODO
                return null;
            }

            @Override
            public Object visit(TestCase.InferenceFailure instance) {
                // TODO
                return null;
            }

            @Override
            public Object visit(TestCase.JsonCoder instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.JsonDecode instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.JsonEncode instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.JsonParser instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.JsonRoundtrip instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.JsonWriter instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.LiftLambdaAboveLet instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.Serialization instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.SimplifyTerm instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.TopologicalSort instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.TopologicalSortBindings instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.TopologicalSortSCC instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.TypeChecking instance) {
                // TODO
                return null;
            }

            @Override
            public Object visit(TestCase.TypeCheckingFailure instance) {
                // TODO
                return null;
            }

            @Override
            public Object visit(TestCase.TypeReduction instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.NormalizeTypeVariables instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.FoldOverTerm instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.RewriteTerm instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.RewriteType instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.HoistSubterms instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.HoistCaseStatements instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.HoistLetBindings instance) {
                return null;
            }

            @Override
            public Object visit(TestCase.HoistPolymorphicLetBindings instance) {
                return null;
            }
        });
    }

    private static void addTestGroup(TestGroup testGroup, String prefix, List<Arguments> args) {
        String newPrefix = testGroupDescription(testGroup, prefix);
        int idx = 0;
        for (TestCaseWithMetadata testCase : testGroup.cases) {
            addTestCase(testCase, ++idx, newPrefix, args);
        }
        for (TestGroup subgroup : testGroup.subgroups) {
            addTestGroup(subgroup, newPrefix, args);
        }
    }

    private static String testCaseDescription(TestCaseWithMetadata testCase, String prefix, int idx) {
        String name = testCase.description.orElse("test #" + idx);
        return prefix == null ? name : prefix + " > " + name;
    }

    private static String testGroupDescription(TestGroup testGroup, String prefix) {
        String name = testGroup.name + testGroup.description.map(s -> " (" + s + ")").orElse("");
        return prefix == null ? name : prefix + " > " + name;
    }
}
