package hydra.lib;

import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Types;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.graph.Graph;
import hydra.util.Either;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Comparator;
import java.util.function.BiFunction;

import static hydra.dsl.Types.variable;

/**
 * A wrapper for a type which can be used as the basis of equality and comparison primitives.
 *
 * @param <T> the Java type corresponding to this primitive type
 */
public class PrimitiveType<T> {
    public final String name;
    public final Type type;
    public final BiFunction<Graph, Term, Either<Error_, T>> expect;
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
                         BiFunction<Graph, Term, Either<Error_, T>> expect,
                         Comparator<T> comparator) {
        this.name = name;
        this.type = type;
        this.expect = expect;
        this.comparator = comparator;
    }

    public static PrimitiveType<Boolean> boolean_() {
        return new PrimitiveType<>("boolean", Types.boolean_(), hydra.extract.Core::boolean_, Boolean::compareTo);
    }

    public static PrimitiveType<BigDecimal> bigfloat() {
        return new PrimitiveType<>("bigfloat", Types.bigfloat(), hydra.extract.Core::bigfloat, BigDecimal::compareTo);
    }

    public static PrimitiveType<BigInteger> bigint() {
        return new PrimitiveType<>("bigint", Types.bigint(), hydra.extract.Core::bigint, BigInteger::compareTo);
    }

    public static PrimitiveType<String> binary() {
        return new PrimitiveType<>("binary", Types.binary(),
            (graph, t) -> hydra.lib.eithers.Map.apply(
                bytes -> new String(bytes, java.nio.charset.StandardCharsets.UTF_8),
                hydra.extract.Core.binary(graph, t)),
            String::compareTo);
    }

    public static PrimitiveType<Float> float32() {
        return new PrimitiveType<>("float32", Types.float32(), hydra.extract.Core::float32, Float::compareTo);
    }

    public static PrimitiveType<Double> float64() {
        return new PrimitiveType<>("float64", Types.float64(), hydra.extract.Core::float64, Double::compareTo);
    }

    public static PrimitiveType<Byte> int8() {
        return new PrimitiveType<>("int8", Types.int8(), hydra.extract.Core::int8, Byte::compareTo);
    }

    public static PrimitiveType<Short> int16() {
        return new PrimitiveType<>("int16", Types.int16(), hydra.extract.Core::int16, Short::compareTo);
    }

    public static PrimitiveType<Integer> int32() {
        return new PrimitiveType<>("int32", Types.int32(), hydra.extract.Core::int32, Integer::compareTo);
    }

    public static PrimitiveType<Long> int64() {
        return new PrimitiveType<>("int64", Types.int64(), hydra.extract.Core::int64, Long::compareTo);
    }

    public static PrimitiveType<String> string() {
        return new PrimitiveType<>("string", Types.string(), hydra.extract.Core::string, String::compareTo);
    }

    public static PrimitiveType<Term> term() {
        return new PrimitiveType<>(
                "term",
                hydra.dsl.Types.apply(variable(Term.TYPE_), variable("a")),
                (graph, t) -> Either.right(t),
                (a, b) -> {
                    throw new UnsupportedOperationException("Term comparison is not yet supported");
                });
    }

    public static PrimitiveType<Type> type() {
        return new PrimitiveType<>(
                "type",
                hydra.dsl.Types.apply(variable(Type.TYPE_), variable("a")),
                (graph, t) -> Either.left(new Error_.Other(new OtherError("Core type decoding not yet implemented"))),
                (a, b) -> {
                    throw new UnsupportedOperationException("Type comparison is not yet supported");
                });
    }

    public static PrimitiveType<Short> uint8() {
        return new PrimitiveType<>("uint8", Types.uint8(), hydra.extract.Core::uint8, Short::compareTo);
    }

    public static PrimitiveType<Character> uint16() {
        return new PrimitiveType<>("uint16", Types.uint16(), hydra.extract.Core::uint16, Character::compareTo);
    }

    public static PrimitiveType<Long> uint32() {
        return new PrimitiveType<>("uint32", Types.uint32(), hydra.extract.Core::uint32, Long::compareTo);
    }

    public static PrimitiveType<BigInteger> uint64() {
        return new PrimitiveType<>("uint64", Types.uint64(), hydra.extract.Core::uint64, BigInteger::compareTo);
    }
}
