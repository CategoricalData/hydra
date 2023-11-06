package hydra;

import hydra.compute.Kv;
import hydra.core.Term;
import hydra.lib.strings.SplitOn;
import org.junit.jupiter.api.Test;

import static hydra.dsl.Terms.apply;
import static hydra.dsl.Terms.int32;
import static hydra.dsl.Terms.lambda;
import static hydra.dsl.Terms.list;
import static hydra.dsl.Terms.string;
import static hydra.dsl.Terms.variable;


/**
 * Just a few simple tests as a sanity check for term reduction. The common test suite provides more rigorous tests.
 */
public class ReductionTest extends HydraTestBase {
    @Test
    public void checkEagerReduction() {
        int i = 0;
        checkEager(++i,
            int32(42),
            int32(42));
        checkEager(++i,
            list(int32(42)),
            list(int32(42)));
        checkEager(++i,
            apply(lambda("x", variable("x")),int32(42)),
            int32(42));

        checkEager(++i,
            apply(apply((new SplitOn<Kv>()).term(), string("ss")), string("Mississippi")),
            list(string("Mi"), string("i"), string("ippi")));
    }

    @Test
    public void checkIncompleteApplicationsAreNotReduced() {
        int i = 0;
        checkEager(++i,
            apply(int32(42), int32(42)),
            apply(int32(42), int32(42)));
    }

    private static void checkEager(int idx, Term<Kv> input, Term<Kv> output) {
        TestSuiteRunner.runReductionTestCase(true, "" + idx, input, output);
    }
}
