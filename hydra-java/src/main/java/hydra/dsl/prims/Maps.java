package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.maps.Alter;
import hydra.lib.maps.Bimap;
import hydra.lib.maps.Elems;
import hydra.lib.maps.Empty;
import hydra.lib.maps.Filter;
import hydra.lib.maps.FilterWithKey;
import hydra.lib.maps.FindWithDefault;
import hydra.lib.maps.FromList;
import hydra.lib.maps.Insert;
import hydra.lib.maps.IsEmpty;
import hydra.lib.maps.Keys;
import hydra.lib.maps.Lookup;
import hydra.lib.maps.Map;
import hydra.lib.maps.MapKeys;
import hydra.lib.maps.Member;
import hydra.lib.maps.Delete;
import hydra.lib.maps.Singleton;
import hydra.lib.maps.Size;
import hydra.lib.maps.ToList;
import hydra.lib.maps.Union;

/**
 * DSL interface providing map primitive operations.
 */
public interface Maps {
    /**
     * Returns a term representing the alter primitive operation for maps.
     *
     * @return a term for altering a value at a key using a function
     */
    static Term alter() {
        return new Alter().term();
    }

    /**
     * Returns a term representing the bimap primitive operation for maps.
     *
     * @return a term for mapping over both keys and values
     */
    static Term bimap() {
        return new Bimap().term();
    }

    /**
     * Returns a term representing the remove primitive operation for maps.
     *
     * @return a term for removing a key-value pair from a map
     */
    static Term delete() {
        return new Delete().term();
    }

    /**
     * Returns a term representing the elems primitive operation for maps.
     *
     * @return a term for getting all values from a map
     */
    static Term elems() {
        return new Elems().term();
    }

    /**
     * Returns a term representing the empty primitive operation for maps.
     *
     * @return a term for creating an empty map
     */
    static Term empty() {
        return new Empty().term();
    }

    /**
     * Returns a term representing the filter primitive operation for maps.
     *
     * @return a term for filtering map values by a predicate
     */
    static Term filter() {
        return new Filter().term();
    }

    /**
     * Returns a term representing the filterWithKey primitive operation for maps.
     *
     * @return a term for filtering map entries by a predicate on keys and values
     */
    static Term filterWithKey() {
        return new FilterWithKey().term();
    }

    /**
     * Returns a term representing the findWithDefault primitive operation for maps.
     *
     * @return a term for looking up a value with a default fallback
     */
    static Term findWithDefault() {
        return new FindWithDefault().term();
    }

    /**
     * Returns a term representing the fromList primitive operation for maps.
     *
     * @return a term for creating a map from a list of key-value pairs
     */
    static Term fromList() {
        return new FromList().term();
    }

    /**
     * Returns a term representing the insert primitive operation for maps.
     *
     * @return a term for inserting a key-value pair into a map
     */
    static Term insert() {
        return new Insert().term();
    }

    /**
     * Returns a term representing the isEmpty primitive operation for maps.
     *
     * @return a term for testing if a map is empty
     */
    static Term isEmpty() {
        return new IsEmpty().term();
    }

    /**
     * Returns a term representing the keys primitive operation for maps.
     *
     * @return a term for getting all keys from a map
     */
    static Term keys() {
        return new Keys().term();
    }

    /**
     * Returns a term representing the lookup primitive operation for maps.
     *
     * @return a term for looking up a value by key
     */
    static Term lookup() {
        return new Lookup().term();
    }

    /**
     * Returns a term representing the map primitive operation for maps.
     *
     * @return a term for mapping a function over map values
     */
    static Term map() {
        return new Map().term();
    }

    /**
     * Returns a term representing the mapKeys primitive operation for maps.
     *
     * @return a term for mapping a function over map keys
     */
    static Term mapKeys() {
        return new MapKeys().term();
    }

    /**
     * Returns a term representing the member primitive operation for maps.
     *
     * @return a term for testing if a key exists in a map
     */
    static Term member() {
        return new Member().term();
    }

    /**
     * Returns a term representing the singleton primitive operation for maps.
     *
     * @return a term for creating a map with a single key-value pair
     */
    static Term singleton() {
        return new Singleton().term();
    }

    /**
     * Returns a term representing the size primitive operation for maps.
     *
     * @return a term for getting the number of entries in a map
     */
    static Term size() {
        return new Size().term();
    }

    /**
     * Returns a term representing the toList primitive operation for maps.
     *
     * @return a term for converting a map to a list of key-value pairs
     */
    static Term toList() {
        return new ToList().term();
    }

    /**
     * Returns a term representing the union primitive operation for maps.
     *
     * @return a term for combining two maps (preferring left on conflicts)
     */
    static Term union() {
        return new Union().term();
    }
}
