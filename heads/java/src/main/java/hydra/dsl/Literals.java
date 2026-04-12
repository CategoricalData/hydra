package hydra.dsl;

import hydra.core.FloatValue;
import hydra.core.IntegerValue;
import hydra.core.Literal;

import java.math.BigDecimal;
import java.math.BigInteger;


/**
 * DSL functions for working with literal values.
 */
public interface Literals {

    /**
     * Create a bigfloat literal from a BigDecimal value.
     *
     * @param value the BigDecimal value
     * @return a bigfloat literal
     */
    static Literal bigfloat(final BigDecimal value) {
        return float_(new FloatValue.Bigfloat(value));
    }

    /**
     * Create a bigint literal from a BigInteger value.
     *
     * @param value the BigInteger value
     * @return a bigint literal
     */
    static Literal bigint(final BigInteger value) {
        return integer(new IntegerValue.Bigint(value));
    }

    /**
     * Create a binary literal from a string value.
     *
     * @param value the binary string value
     * @return a binary literal
     */
    static Literal binary(final String value) {
        return new Literal.Binary(value.getBytes(java.nio.charset.StandardCharsets.UTF_8));
    }

    /**
     * Create a boolean literal from a boolean value.
     *
     * @param value the boolean value
     * @return a boolean literal
     */
    static Literal boolean_(final boolean value) {
        return new Literal.Boolean_(value);
    }

    /**
     * Create a float literal from a FloatValue.
     *
     * @param value the float value
     * @return a float literal
     */
    static Literal float_(final FloatValue value) {
        return new Literal.Float_(value);
    }

    /**
     * Create a float32 literal from a float value.
     *
     * @param value the float value
     * @return a float32 literal
     */
    static Literal float32(final float value) {
        return float_(new FloatValue.Float32(value));
    }

    /**
     * Create a float64 literal from a double value.
     *
     * @param value the double value
     * @return a float64 literal
     */
    static Literal float64(final double value) {
        return float_(new FloatValue.Float64(value));
    }

    /**
     * Create an int8 literal from a byte value.
     *
     * @param value the byte value
     * @return an int8 literal
     */
    static Literal int8(final byte value) {
        return integer(new IntegerValue.Int8(value));
    }

    /**
     * Create an int16 literal from a short value.
     *
     * @param value the short value
     * @return an int16 literal
     */
    static Literal int16(final short value) {
        return integer(new IntegerValue.Int16(value));
    }

    /**
     * Create an int32 literal from an int value.
     *
     * @param value the int value
     * @return an int32 literal
     */
    static Literal int32(final int value) {
        return integer(new IntegerValue.Int32(value));
    }

    /**
     * Create an int64 literal from a long value.
     *
     * @param value the long value
     * @return an int64 literal
     */
    static Literal int64(final long value) {
        return integer(new IntegerValue.Int64(value));
    }

    /**
     * Create an integer literal from an IntegerValue.
     *
     * @param value the integer value
     * @return an integer literal
     */
    static Literal integer(final IntegerValue value) {
        return new Literal.Integer_(value);
    }

    /**
     * Create a string literal from a string value.
     *
     * @param value the string value
     * @return a string literal
     */
    static Literal string(final String value) {
        return new Literal.String_(value);
    }

    /**
     * Create a uint8 literal from a short value.
     *
     * @param value the short value
     * @return a uint8 literal
     */
    static Literal uint8(final short value) {
        return integer(new IntegerValue.Uint8(value));
    }

    /**
     * Create a uint16 literal from a char value.
     *
     * @param value the char value
     * @return a uint16 literal
     */
    static Literal uint16(final char value) {
        return integer(new IntegerValue.Uint16(value));
    }

    /**
     * Create a uint32 literal from a long value.
     *
     * @param value the long value
     * @return a uint32 literal
     */
    static Literal uint32(final long value) {
        return integer(new IntegerValue.Uint32(value));
    }

    /**
     * Create a uint64 literal from a long value.
     *
     * @param value the long value
     * @return a uint64 literal
     */
    static Literal uint64(final long value) {
        return uint64(BigInteger.valueOf(value));
    }

    /**
     * Create a uint64 literal from a BigInteger value.
     *
     * @param value the BigInteger value
     * @return a uint64 literal
     */
    static Literal uint64(final BigInteger value) {
        return integer(new IntegerValue.Uint64(value));
    }

    /**
     * Encode a literal value as a string.
     * @param value the literal value to encode
     * @return a string representation of the literal
     */
    static String showLiteral(Literal value) {
        return value.accept(new Literal.Visitor<String>() {
            @Override
            public String visit(Literal.Binary instance) {
                return "binary:" + instance.value;
            }

            @Override
            public String visit(Literal.Boolean_ instance) {
                return "boolean:" + instance.value;
            }

            @Override
            public String visit(Literal.Float_ instance) {
                return "float:" + instance.value.accept(new FloatValue.Visitor<String>() {
                    @Override
                    public String visit(FloatValue.Bigfloat instance) {
                        return "bigfloat:" + instance.value;
                    }

                    @Override
                    public String visit(FloatValue.Float32 instance) {
                        return "float32:" + instance.value;
                    }

                    @Override
                    public String visit(FloatValue.Float64 instance) {
                        return "float64:" + instance.value;
                    }
                });
            }

            @Override
            public String visit(Literal.Integer_ instance) {
                return "integer:" + instance.value.accept(new IntegerValue.Visitor<String>() {
                    @Override
                    public String visit(IntegerValue.Bigint instance) {
                        return "bigint:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Int16 instance) {
                        return "int16:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Int32 instance) {
                        return "int32:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Int64 instance) {
                        return "int64:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Int8 instance) {
                        return "int8:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Uint16 instance) {
                        return "uint16:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Uint32 instance) {
                        return "uint32:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Uint64 instance) {
                        return "uint64:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Uint8 instance) {
                        return "uint8:" + instance.value;
                    }
                });
            }

            @Override
            public String visit(Literal.String_ instance) {
                return "string:\"" + instance.value + "\"";
            }
        });
    }
}
