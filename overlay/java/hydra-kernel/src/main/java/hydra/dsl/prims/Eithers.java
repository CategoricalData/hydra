package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.eithers.Bimap;
import hydra.lib.eithers.Bind;
import hydra.lib.eithers.Either;
import hydra.lib.eithers.FromLeft;
import hydra.lib.eithers.FromRight;
import hydra.lib.eithers.IsLeft;
import hydra.lib.eithers.IsRight;
import hydra.lib.eithers.Lefts;
import hydra.lib.eithers.PartitionEithers;
import hydra.lib.eithers.Rights;

/**
 * DSL interface providing either primitive operations.
 */
public interface Eithers {
    /**
     * Returns a term representing the bimap primitive operation for eithers.
     * Applies two functions to the left and right cases respectively.
     *
     * @return a term for bimap over eithers
     */
    static Term bimap() {
        return new Bimap().term();
    }

    /**
     * Returns a term representing the bind primitive operation for eithers.
     * Monadic bind for the Either type.
     *
     * @return a term for monadic bind in an either context
     */
    static Term bind() {
        return new Bind().term();
    }

    /**
     * Returns a term representing the either primitive operation.
     * Case analysis for the Either type.
     *
     * @return a term for case analysis over eithers
     */
    static Term either() {
        return new Either().term();
    }

    /**
     * Returns a term representing the fromLeft primitive operation.
     * Extracts the value from a Left, using a default if Right.
     *
     * @return a term for extracting the left value
     */
    static Term fromLeft() {
        return new FromLeft().term();
    }

    /**
     * Returns a term representing the fromRight primitive operation.
     * Extracts the value from a Right, using a default if Left.
     *
     * @return a term for extracting the right value
     */
    static Term fromRight() {
        return new FromRight().term();
    }

    /**
     * Returns a term representing the isLeft primitive operation.
     * Tests if an Either is a Left value.
     *
     * @return a term for testing if left
     */
    static Term isLeft() {
        return new IsLeft().term();
    }

    /**
     * Returns a term representing the isRight primitive operation.
     * Tests if an Either is a Right value.
     *
     * @return a term for testing if right
     */
    static Term isRight() {
        return new IsRight().term();
    }

    /**
     * Returns a term representing the lefts primitive operation.
     * Extracts all Left values from a list of Eithers.
     *
     * @return a term for extracting lefts
     */
    static Term lefts() {
        return new Lefts().term();
    }

    /**
     * Returns a term representing the partitionEithers primitive operation.
     * Partitions a list of Eithers into a pair of lists.
     *
     * @return a term for partitioning eithers
     */
    static Term partitionEithers() {
        return new PartitionEithers().term();
    }

    /**
     * Returns a term representing the rights primitive operation.
     * Extracts all Right values from a list of Eithers.
     *
     * @return a term for extracting rights
     */
    static Term rights() {
        return new Rights().term();
    }
}
