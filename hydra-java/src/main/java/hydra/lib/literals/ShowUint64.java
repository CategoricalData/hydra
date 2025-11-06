package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.uint64;


/**
 * Primitive function: ShowUint64.
 */
public class ShowUint64 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.showUint64");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(uint64(), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.bigint(args.get(0)),
            (Function<BigInteger, Term>) i -> Terms.string(apply(i)));
    }

    /**
     * Applies the ShowUint64 operation.
     * @param value the value
     * @return the result
     */
        public static String apply(BigInteger value) {
        return value.toString();
    }
}
