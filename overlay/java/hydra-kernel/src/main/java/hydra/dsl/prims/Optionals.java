package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.optionals.Apply;
import hydra.lib.optionals.Bind;
import hydra.lib.optionals.Map;
import hydra.lib.optionals.Pure;

/**
 * DSL interface providing optional primitive operations for working with optional values.
 */
public interface Optionals {
    /**
     * Returns a term representing the apply primitive operation for maybes.
     *
     * @return a term for applying a function in a maybe context
     */
    static Term apply() {
        return new Apply().term();
    }

    /**
     * Returns a term representing the bind primitive operation for maybes.
     *
     * @return a term for monadic bind in a maybe context
     */
    static Term bind() {
        return new Bind().term();
    }

    /**
     * Returns a term representing the map primitive operation for maybes.
     *
     * @return a term for mapping a function over a maybe value
     */
    static Term map() {
        return new Map().term();
    }

    /**
     * Returns a term representing the pure primitive operation for maybes.
     *
     * @return a term for lifting a value into a maybe context
     */
    static Term pure() {
        return new Pure().term();
    }
}
