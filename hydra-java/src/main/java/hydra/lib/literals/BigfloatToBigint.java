package hydra.lib.literals;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;

/**
 * Primitive function which converts a bigfloat (arbitrary-precision decimal) to a bigint (arbitrary-precision integer).
 * This conversion truncates any fractional part of the decimal value.
 */
public class BigfloatToBigint extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigfloatToBigint"
     */
    public Name name() {
        return new Name("hydra.lib.literals.bigfloatToBigint");
    }

    /**
     * Returns the type scheme for this function: bigfloat -&gt; bigint.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigfloat(), Types.bigint()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigfloat terms to bigint terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.bigfloat(args.get(0)), s -> Terms.bigint(apply(s)));
    }

    /**
     * Converts a BigDecimal value to a BigInteger by truncating the fractional part.
     * @param value the BigDecimal value to convert
     * @return the BigInteger representation of the value
     */
    public static BigInteger apply(BigDecimal value) {
        return value.toBigInteger();
    }
}
