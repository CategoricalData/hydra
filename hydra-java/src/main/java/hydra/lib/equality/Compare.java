package hydra.lib.equality;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.*;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Comparison;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Compares two values and returns a Comparison (LessThan, EqualTo, or GreaterThan).
 */
public class Compare extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.equality.compare");
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function(Types.var("x"), Types.var("x"), Types.apply(Types.var("hydra.util.Comparison"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            // For Term comparison, we use a simple comparison based on the term structure
            // This is a simplified implementation; a full implementation would need proper Ord instance
            int cmp = compareTerms(args.get(0), args.get(1));
            if (cmp < 0) {
                return Flows.pure(Terms.injectUnit(Comparison.TYPE_NAME, Comparison.FIELD_NAME_LESS_THAN));
            } else if (cmp > 0) {
                return Flows.pure(Terms.injectUnit(Comparison.TYPE_NAME, Comparison.FIELD_NAME_GREATER_THAN));
            } else {
                return Flows.pure(Terms.injectUnit(Comparison.TYPE_NAME, Comparison.FIELD_NAME_EQUAL_TO));
            }
        };
    }

    /**
     * Compares two terms structurally.
     * For literal values, compares by value (numeric comparison for numbers).
     * For other terms, falls back to show representation comparison.
     */
    public static int compareTerms(Term t1, Term t2) {
        // Try to extract and compare literal values directly
        if (t1 instanceof Term.Literal && t2 instanceof Term.Literal) {
            Literal l1 = ((Term.Literal) t1).value;
            Literal l2 = ((Term.Literal) t2).value;
            return compareLiterals(l1, l2);
        }
        // Fallback: compare show representations
        return hydra.show.core.Core.term(t1).compareTo(hydra.show.core.Core.term(t2));
    }

    private static int compareLiterals(Literal l1, Literal l2) {
        // Same type comparisons
        if (l1 instanceof Literal.Integer_ && l2 instanceof Literal.Integer_) {
            return compareIntegers(((Literal.Integer_) l1).value, ((Literal.Integer_) l2).value);
        }
        if (l1 instanceof Literal.Float_ && l2 instanceof Literal.Float_) {
            return compareFloats(((Literal.Float_) l1).value, ((Literal.Float_) l2).value);
        }
        if (l1 instanceof Literal.String_ && l2 instanceof Literal.String_) {
            return ((Literal.String_) l1).value.compareTo(((Literal.String_) l2).value);
        }
        if (l1 instanceof Literal.Boolean_ && l2 instanceof Literal.Boolean_) {
            return Boolean.compare(((Literal.Boolean_) l1).value, ((Literal.Boolean_) l2).value);
        }
        // Different literal types: use show representation
        return hydra.show.core.Core.literal(l1).compareTo(hydra.show.core.Core.literal(l2));
    }

    @SuppressWarnings("unchecked")
    private static int compareIntegers(IntegerValue v1, IntegerValue v2) {
        // Convert both to BigInteger for cross-type comparison
        BigInteger b1 = integerToBigInteger(v1);
        BigInteger b2 = integerToBigInteger(v2);
        if (b1 != null && b2 != null) {
            return b1.compareTo(b2);
        }
        return v1.toString().compareTo(v2.toString());
    }

    private static BigInteger integerToBigInteger(IntegerValue v) {
        if (v instanceof IntegerValue.Int8) return BigInteger.valueOf(((IntegerValue.Int8) v).value);
        if (v instanceof IntegerValue.Int16) return BigInteger.valueOf(((IntegerValue.Int16) v).value);
        if (v instanceof IntegerValue.Int32) return BigInteger.valueOf(((IntegerValue.Int32) v).value);
        if (v instanceof IntegerValue.Int64) return BigInteger.valueOf(((IntegerValue.Int64) v).value);
        if (v instanceof IntegerValue.Uint8) return BigInteger.valueOf(((IntegerValue.Uint8) v).value);
        if (v instanceof IntegerValue.Uint16) return BigInteger.valueOf((int) ((IntegerValue.Uint16) v).value);
        if (v instanceof IntegerValue.Uint32) return BigInteger.valueOf(((IntegerValue.Uint32) v).value);
        if (v instanceof IntegerValue.Uint64) return ((IntegerValue.Uint64) v).value;
        if (v instanceof IntegerValue.Bigint) return ((IntegerValue.Bigint) v).value;
        return null;
    }

    private static int compareFloats(FloatValue v1, FloatValue v2) {
        BigDecimal d1 = floatToBigDecimal(v1);
        BigDecimal d2 = floatToBigDecimal(v2);
        if (d1 != null && d2 != null) {
            return d1.compareTo(d2);
        }
        return v1.toString().compareTo(v2.toString());
    }

    private static BigDecimal floatToBigDecimal(FloatValue v) {
        if (v instanceof FloatValue.Float32) return BigDecimal.valueOf(((FloatValue.Float32) v).value);
        if (v instanceof FloatValue.Float64) return BigDecimal.valueOf(((FloatValue.Float64) v).value);
        if (v instanceof FloatValue.Bigfloat) return new BigDecimal(((FloatValue.Bigfloat) v).value.toString());
        return null;
    }

    /**
     * Compares two comparable values.
     * @param <A> the comparable type
     * @param left the first value
     * @return a function that takes the second value and returns the comparison result
     */
    @SuppressWarnings("unchecked")
    public static <A> Function<A, Comparison> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Compares two comparable values.
     * @param <A> the comparable type
     * @param left the first value
     * @param right the second value
     * @return the comparison result
     */
    @SuppressWarnings("unchecked")
    public static <A> Comparison apply(A left, A right) {
        int cmp = ((Comparable) left).compareTo(right);
        if (cmp < 0) {
            return new Comparison.LessThan();
        } else if (cmp > 0) {
            return new Comparison.GreaterThan();
        } else {
            return new Comparison.EqualTo();
        }
    }
}
