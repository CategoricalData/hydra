package hydra.dsl;

// import hydra.basics.Basics; // TODO: restore when kernel terms modules are generated
import hydra.core.FloatValue;
import hydra.core.IntegerValue;
import hydra.core.Literal;
import hydra.core.LiteralType;

import java.math.BigInteger;
import hydra.util.Opt;


/**
 * DSL functions for working with literal values.
 */
public interface Literals {

    static Literal bigfloat(final double value) {
        return float_(new FloatValue.Bigfloat(String.valueOf(value)));
    }

    static Literal bigint(final BigInteger value) {
        return integer(new IntegerValue.Bigint(value));
    }

    static Literal binary(final String value) {
        return new Literal.Binary(value);
    }

    static Literal boolean_(final boolean value) {
        return new Literal.Boolean_(value);
    }

    static Literal float_(final FloatValue value) {
        return new Literal.Float_(value);
    }

    static Literal float32(final float value) {
        return float_(new FloatValue.Float32(value));
    }

    static Literal float64(final double value) {
        return float_(new FloatValue.Float64(value));
    }

    static Literal int8(final byte value) {
        return integer(new IntegerValue.Int8(value));
    }

    static Literal int16(final short value) {
        return integer(new IntegerValue.Int16(value));
    }

    static Literal int32(final int value) {
        return integer(new IntegerValue.Int32(value));
    }

    static Literal int64(final long value) {
        return integer(new IntegerValue.Int64(value));
    }

    static Literal integer(final IntegerValue value) {
        return new Literal.Integer_(value);
    }

    static Literal string(final String value) {
        return new Literal.String_(value);
    }

    static Literal uint8(final char value) {
        return integer(new IntegerValue.Uint8((short) value));
    }

    static Literal uint16(final char value) {
        return integer(new IntegerValue.Uint16(value));
    }

    static Literal uint32(final long value) {
        return integer(new IntegerValue.Uint32(value));
    }

    static Literal uint64(final long value) {
        return uint64(BigInteger.valueOf(value));
    }

    static Literal uint64(final BigInteger value) {
        return integer(new IntegerValue.Uint64(value));
    }

    /**
     * Encode a literal value as a string.
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

    // TODO: restore when kernel terms modules (Basics) are generated
    // /**
    //  * Check a literal value against an expected type.
    //  */
    // static Opt<String> checkLiteral(LiteralType type, Literal value) {
    //     String expected = LiteralTypes.showLiteralType(type);
    //     String actual = LiteralTypes.showLiteralType(Basics.literalType(value));
    //     return expected.equals(actual)
    //             ? Opt.empty()
    //             : Opt.of("Expected literal of type " + expected + ", found " + actual);
    // }
}
