package hydra.overlay.java.dsl.prims;

import hydra.core.Term;
import hydra.overlay.java.lib.lists.Apply;
import hydra.overlay.java.lib.lists.Bind;
import hydra.overlay.java.lib.lists.Concat;
import hydra.overlay.java.lib.lists.Intercalate;
import hydra.overlay.java.lib.lists.Intersperse;
import hydra.overlay.java.lib.lists.Length;
import hydra.overlay.java.lib.lists.Map;
import hydra.overlay.java.lib.lists.Pure;

/**
 * DSL interface providing list primitive operations.
 */
public interface Lists {
    /**
     * Returns a term representing the apply primitive operation for lists.
     *
     * @return a term for applying a function in a list context
     */
    static Term apply() {
        return new Apply().term();
    }

    /**
     * Returns a term representing the bind primitive operation for lists.
     *
     * @return a term for monadic bind in a list context
     */
    static Term bind() {
        return new Bind().term();
    }

    /**
     * Returns a term representing the concat primitive operation for lists.
     *
     * @return a term for concatenating lists
     */
    static Term concat() {
        return new Concat().term();
    }

    /**
     * Returns a term representing the intercalate primitive operation for lists.
     *
     * @return a term for inserting a list between lists and concatenating
     */
    static Term intercalate() {
        return new Intercalate().term();
    }

    /**
     * Returns a term representing the intersperse primitive operation for lists.
     *
     * @return a term for inserting an element between list elements
     */
    static Term intersperse() {
        return new Intersperse().term();
    }

    /**
     * Returns a term representing the length primitive operation for lists.
     *
     * @return a term for getting the length of a list
     */
    static Term length() {
        return new Length().term();
    }

    /**
     * Returns a term representing the map primitive operation for lists.
     *
     * @return a term for mapping a function over a list
     */
    static Term map() {
        return new Map().term();
    }

    /**
     * Returns a term representing the pure primitive operation for lists.
     *
     * @return a term for lifting a value into a list context
     */
    static Term pure() {
        return new Pure().term();
    }
}
