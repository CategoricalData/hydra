package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.maybes.Apply;
import hydra.lib.maybes.Bind;
import hydra.lib.maybes.Map;
import hydra.lib.maybes.Pure;

/**
 * DSL interface providing Maybe/Optional primitive operations for working with optional values.
 */
public interface Maybes {
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
