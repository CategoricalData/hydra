package hydra.lib.io;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.variable;

/**
 * Converts a term to its string representation.
 */
public class ShowTerm extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.lib.io.showTerm"
     */
    public Name name() {
        return new Name("hydra.lib.io.showTerm");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme representing a term to string function
     */
    @Override
    public TypeScheme type() {
        return scheme(function(variable(Term.TYPE_NAME), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that takes a list of terms and returns a flow producing a string term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.pure(Terms.string(ShowTerm.apply(args.get(0))));
    }

    /**
     * Converts a term to its string representation.
     * @param term the term to convert
     * @return the string representation of the term
     */
    public static  String apply(Term term) {
        // TODO: temporary
        return term.toString();
    }
}
