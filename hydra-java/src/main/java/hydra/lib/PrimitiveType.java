package hydra.lib;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Types;
import hydra.graph.Graph;

import java.math.BigInteger;
import java.util.Comparator;
import java.util.function.Function;

import static hydra.dsl.Types.variable;

/**
 * A wrapper for a type which can be used as the basis of equality and comparison primitives.
 */
public class PrimitiveType<T> {
    public final String name;
    public final Type type;
    public final Function<Term, Flow<Graph, T>> expect;
    public final Comparator<T> comparator;

    /**
     * Construct a primitive type.
     */
    public PrimitiveType(String name,
                         Type type,
                         Function<Term, Flow<Graph, T>> expect,
                         Comparator<T> comparator) {
        this.name = name;
        this.type = type;
        this.expect = expect;
        this.comparator = comparator;
    }

    /**
     * The primitive type for boolean literal values.
     */
    public static  PrimitiveType<Boolean> boolean_() {
        return new PrimitiveType<Boolean>("boolean", Types.boolean_(), Expect::boolean_, Boolean::compareTo);
    }

    /**
     * The primitive type for bigfloat literal values.
     */
    public static  PrimitiveType<Double> bigfloat() {
        return new PrimitiveType<Double>("bigfloat", Types.bigfloat(), Expect::bigfloat, Double::compareTo);
    }

    /**
     * The primitive type for bigint literal values.
     */
    public static  PrimitiveType<BigInteger> bigint() {
        return new PrimitiveType<BigInteger>("bigint", Types.bigint(), Expect::bigint, BigInteger::compareTo);
    }

    /**
     * The primitive type for binary literal values.
     */
    public static  PrimitiveType<String> binary() {
        return new PrimitiveType<String>("binary", Types.binary(), Expect::binary, String::compareTo);
    }

    /**
     * The primitive type for float32 literal values.
     */
    public static  PrimitiveType<Float> float32() {
        return new PrimitiveType<Float>("float32", Types.float32(), Expect::float32, Float::compareTo);
    }

    /**
     * The primitive type for float64 literal values.
     */
    public static  PrimitiveType<Double> float64() {
        return new PrimitiveType<Double>("float64", Types.float64(), Expect::float64, Double::compareTo);
    }

    /**
     * The primitive type for int8 literal values.
     */
    public static  PrimitiveType<Short> int8() {
        return new PrimitiveType<Short>("int8", Types.int8(), Expect::int8, Short::compareTo);
    }

    /**
     * The primitive type for int16 literal values.
     */
    public static  PrimitiveType<Short> int16() {
        return new PrimitiveType<Short>("int16", Types.int16(), Expect::int16, Short::compareTo);
    }

    /**
     * The primitive type for int32 literal values.
     */
    public static  PrimitiveType<Integer> int32() {
        return new PrimitiveType<Integer>("int32", Types.int32(), Expect::int32, Integer::compareTo);
    }

    /**
     * The primitive type for int64 literal values.
     */
    public static  PrimitiveType<Long> int64() {
        return new PrimitiveType<Long>("int64", Types.int64(), Expect::int64, Long::compareTo);
    }

    /**
     * The primitive type for string literal values.
     */
    public static  PrimitiveType<String> string() {
        return new PrimitiveType<String>("string", Types.string(), Expect::string, String::compareTo);
    }

    /**
     * The primitive type for Hydra terms.
     */
    public static  PrimitiveType<Term> term() {
        return new PrimitiveType<Term>(
                "term",
                hydra.dsl.Types.apply(variable(Term.NAME), variable("a")),
                Expect::term,
                (a, b) -> {
                    throw new UnsupportedOperationException("Term comparison is not yet supported");
                });
    }

    /**
     * The primitive type for Hydra types.
     */
    public static  PrimitiveType<Type> type() {
        return new PrimitiveType<Type>(
                "type",
                hydra.dsl.Types.apply(variable(Type.NAME), variable("a")),
                Expect::type,
                (a, b) -> {
                    throw new UnsupportedOperationException("Type comparison is not yet supported");
                });
    }

    /**
     * The primitive type for uint8 literal values.
     */
    public static  PrimitiveType<Byte> uint8() {
        return new PrimitiveType<Byte>("uint8", Types.uint8(), Expect::uint8, Byte::compareTo);
    }

    /**
     * The primitive type for uint16 literal values.
     */
    public static  PrimitiveType<Character> uint16() {
        return new PrimitiveType<Character>("uint16", Types.uint16(), Expect::uint16, Character::compareTo);
    }

    /**
     * The primitive type for uint32 literal values.
     */
    public static  PrimitiveType<Long> uint32() {
        return new PrimitiveType<Long>("uint32", Types.uint32(), Expect::uint32, Long::compareTo);
    }

    /**
     * The primitive type for uint64 literal values.
     */
    public static  PrimitiveType<BigInteger> uint64() {
        return new PrimitiveType<BigInteger>("uint64", Types.uint64(), Expect::uint64, BigInteger::compareTo);
    }
}
