package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.sets.Member;
import hydra.lib.sets.Empty;
import hydra.lib.sets.FromList;
import hydra.lib.sets.Insert;
import hydra.lib.sets.IsEmpty;
import hydra.lib.sets.Map;
import hydra.lib.sets.Delete;
import hydra.lib.sets.Singleton;
import hydra.lib.sets.Size;
import hydra.lib.sets.ToList;

/**
 * DSL interface providing set primitive operations.
 */
public interface Sets {
    /**
     * Returns a term representing the contains (member) primitive operation for sets.
     *
     * @return a term for testing if an element is in a set
     */
    static Term contains() {
        return new Member().term();
    }

    /**
     * Returns a term representing the empty primitive operation for sets.
     *
     * @return a term for creating an empty set
     */
    static Term empty() {
        return new Empty().term();
    }

    /**
     * Returns a term representing the fromList primitive operation for sets.
     *
     * @return a term for creating a set from a list
     */
    static Term fromList() {
        return new FromList().term();
    }

    /**
     * Returns a term representing the insert primitive operation for sets.
     *
     * @return a term for inserting an element into a set
     */
    static Term insert() {
        return new Insert().term();
    }

    /**
     * Returns a term representing the isEmpty primitive operation for sets.
     *
     * @return a term for testing if a set is empty
     */
    static Term isEmpty() {
        return new IsEmpty().term();
    }

    /**
     * Returns a term representing the map primitive operation for sets.
     *
     * @return a term for mapping a function over set elements
     */
    static Term map() {
        return new Map().term();
    }

    /**
     * Returns a term representing the remove (delete) primitive operation for sets.
     *
     * @return a term for removing an element from a set
     */
    static Term remove() {
        return new Delete().term();
    }

    /**
     * Returns a term representing the singleton primitive operation for sets.
     *
     * @return a term for creating a set with a single element
     */
    static Term singleton() {
        return new Singleton().term();
    }

    /**
     * Returns a term representing the size primitive operation for sets.
     *
     * @return a term for getting the number of elements in a set
     */
    static Term size() {
        return new Size().term();
    }

    /**
     * Returns a term representing the toList primitive operation for sets.
     *
     * @return a term for converting a set to a list
     */
    static Term toList() {
        return new ToList().term();
    }
}
