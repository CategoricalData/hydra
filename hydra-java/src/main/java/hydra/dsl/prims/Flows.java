package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.flows.Apply;
import hydra.lib.flows.Bind;
import hydra.lib.flows.Map;
import hydra.lib.flows.Pure;

/**
 * DSL interface providing flow primitive operations for working with monadic computations.
 */
public interface Flows {
    /**
     * Returns a term representing the apply primitive operation for flows.
     *
     * @return a term for applying a function in a flow context
     */
    static Term apply() {
        return new Apply().term();
    }

    /**
     * Returns a term representing the bind primitive operation for flows.
     *
     * @return a term for monadic bind in a flow context
     */
    static Term bind() {
        return new Bind().term();
    }

    /**
     * Returns a term representing the map primitive operation for flows.
     *
     * @return a term for mapping a function over a flow
     */
    static Term map() {
        return new Map().term();
    }

    /**
     * Returns a term representing the pure primitive operation for flows.
     *
     * @return a term for lifting a value into a flow context
     */
    static Term pure() {
        return new Pure().term();
    }
}
