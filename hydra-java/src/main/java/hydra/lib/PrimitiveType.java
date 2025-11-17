package hydra.lib;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Types;
import hydra.graph.Graph;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Comparator;
import java.util.function.Function;

import static hydra.dsl.Types.variable;

/**
 * A wrapper for a type which can be used as the basis of equality and comparison primitives.
 *
 * @param <T> the Java type corresponding to this primitive type
 */
public class PrimitiveType<T> {
    public final String name;
    public final Type type;
    public final Function<Term, Flow<Graph, T>> expect;
    public final Comparator<T> comparator;

    /**
     * Construct a primitive type.
     *
     * @param name the name of this primitive type
     * @param type the Hydra type representation
     * @param expect a function to extract a value of type T from a Term
     * @param comparator a comparator for values of type T
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
     *
     * @return a primitive type for booleans
     */
    public static  PrimitiveType<Boolean> boolean_() {
        return new PrimitiveType<>("boolean", Types.boolean_(), Expect::boolean_, Boolean::compareTo);
    }

    /**
     * The primitive type for bigfloat literal values.
     *
     * @return a primitive type for arbitrary-precision decimals
     */
    public static  PrimitiveType<BigDecimal> bigfloat() {
        return new PrimitiveType<>("bigfloat", Types.bigfloat(), Expect::bigfloat, BigDecimal::compareTo);
    }

    /**
     * The primitive type for bigint literal values.
     *
     * @return a primitive type for arbitrary-precision integers
     */
    public static  PrimitiveType<BigInteger> bigint() {
        return new PrimitiveType<>("bigint", Types.bigint(), Expect::bigint, BigInteger::compareTo);
    }

    /**
     * The primitive type for binary literal values.
     *
     * @return a primitive type for binary data
     */
    public static  PrimitiveType<String> binary() {
        return new PrimitiveType<>("binary", Types.binary(), Expect::binary, String::compareTo);
    }

    /**
     * The primitive type for float32 literal values.
     *
     * @return a primitive type for 32-bit floating-point numbers
     */
    public static  PrimitiveType<Float> float32() {
        return new PrimitiveType<>("float32", Types.float32(), Expect::float32, Float::compareTo);
    }

    /**
     * The primitive type for float64 literal values.
     *
     * @return a primitive type for 64-bit floating-point numbers
     */
    public static  PrimitiveType<Double> float64() {
        return new PrimitiveType<>("float64", Types.float64(), Expect::float64, Double::compareTo);
    }

    /**
     * The primitive type for int8 literal values.
     *
     * @return a primitive type for 8-bit signed integers
     */
    public static  PrimitiveType<Byte> int8() {
        return new PrimitiveType<>("int8", Types.int8(), Expect::int8, Byte::compareTo);
    }

    /**
     * The primitive type for int16 literal values.
     *
     * @return a primitive type for 16-bit signed integers
     */
    public static  PrimitiveType<Short> int16() {
        return new PrimitiveType<>("int16", Types.int16(), Expect::int16, Short::compareTo);
    }

    /**
     * The primitive type for int32 literal values.
     *
     * @return a primitive type for 32-bit signed integers
     */
    public static  PrimitiveType<Integer> int32() {
        return new PrimitiveType<>("int32", Types.int32(), Expect::int32, Integer::compareTo);
    }

    /**
     * The primitive type for int64 literal values.
     *
     * @return a primitive type for 64-bit signed integers
     */
    public static  PrimitiveType<Long> int64() {
        return new PrimitiveType<>("int64", Types.int64(), Expect::int64, Long::compareTo);
    }

    /**
     * The primitive type for string literal values.
     *
     * @return a primitive type for strings
     */
    public static  PrimitiveType<String> string() {
        return new PrimitiveType<>("string", Types.string(), Expect::string, String::compareTo);
    }

    /**
     * The primitive type for Hydra terms.
     *
     * @return a primitive type for Hydra terms
     */
    public static  PrimitiveType<Term> term() {
        return new PrimitiveType<>(
                "term",
                hydra.dsl.Types.apply(variable(Term.TYPE_NAME), variable("a")),
                Expect::term,
                (a, b) -> {
                    throw new UnsupportedOperationException("Term comparison is not yet supported");
                });
    }

    /**
     * The primitive type for Hydra types.
     *
     * @return a primitive type for Hydra types
     */
    public static  PrimitiveType<Type> type() {
        return new PrimitiveType<>(
                "type",
                hydra.dsl.Types.apply(variable(Type.TYPE_NAME), variable("a")),
                Expect::type,
                (a, b) -> {
                    throw new UnsupportedOperationException("Type comparison is not yet supported");
                });
    }

    /**
     * The primitive type for uint8 literal values.
     *
     * @return a primitive type for 8-bit unsigned integers
     */
    public static  PrimitiveType<Short> uint8() {
        return new PrimitiveType<>("uint8", Types.uint8(), Expect::uint8, Short::compareTo);
    }

    /**
     * The primitive type for uint16 literal values.
     *
     * @return a primitive type for 16-bit unsigned integers
     */
    public static  PrimitiveType<Character> uint16() {
        return new PrimitiveType<>("uint16", Types.uint16(), Expect::uint16, Character::compareTo);
    }

    /**
     * The primitive type for uint32 literal values.
     *
     * @return a primitive type for 32-bit unsigned integers
     */
    public static  PrimitiveType<Long> uint32() {
        return new PrimitiveType<>("uint32", Types.uint32(), Expect::uint32, Long::compareTo);
    }

    /**
     * The primitive type for uint64 literal values.
     *
     * @return a primitive type for 64-bit unsigned integers
     */
    public static  PrimitiveType<BigInteger> uint64() {
        return new PrimitiveType<>("uint64", Types.uint64(), Expect::uint64, BigInteger::compareTo);
    }
}
