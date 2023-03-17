package hydra;

import hydra.compute.Kv;
import hydra.lib.strings.SplitOn;
import org.junit.jupiter.api.Test;

import static hydra.dsl.Terms.*;


/**
 * Just a few simple tests as a sanity check for term reduction. The common test suite provides more rigorous tests.
 */
public class ReductionTest extends HydraTestBase {
    @Test
    public void checkEagerReduction() {
        int i = 0;
        TestSuiteRunner.runReductionTestCase(true, "" + ++i,
            int32(42),
            int32(42));
        TestSuiteRunner.runReductionTestCase(true, "" + ++i,
            list(int32(42)),
            list(int32(42)));
        TestSuiteRunner.runReductionTestCase(true, "" + ++i,
            apply(lambda("x", variable("x")),int32(42)),
            int32(42));

        TestSuiteRunner.runReductionTestCase(true, "" + ++i,
            apply(apply((new SplitOn<Kv>()).term(), string("ss")), string("Mississippi")),
            list(string("Mi"), string("i"), string("ippi")));
    }
}
