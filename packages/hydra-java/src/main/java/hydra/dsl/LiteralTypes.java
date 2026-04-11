package hydra.dsl;

import hydra.core.FloatType;
import hydra.core.IntegerType;
import hydra.core.LiteralType;


/**
 * DSL utilities for working with literal types.
 */
public interface LiteralTypes {
    /**
     * Create a bigfloat literal type.
     *
     * @return a bigfloat literal type
     */
    static LiteralType bigfloat() {
        return float_(new FloatType.Bigfloat());
    }

    /**
     * Create a bigint literal type.
     *
     * @return a bigint literal type
     */
    static LiteralType bigint() {
        return integer(new IntegerType.Bigint());
    }

    /**
     * Create a binary literal type.
     *
     * @return a binary literal type
     */
    static LiteralType binary() {
        return new LiteralType.Binary();
    }

    /**
     * Create a boolean literal type.
     *
     * @return a boolean literal type
     */
    static LiteralType boolean_() {
        return new LiteralType.Boolean_();
    }

    /**
     * Create a float32 literal type.
     *
     * @return a float32 literal type
     */
    static LiteralType float32() {
        return float_(new FloatType.Float32());
    }

    /**
     * Create a float64 literal type.
     *
     * @return a float64 literal type
     */
    static LiteralType float64() {
        return float_(new FloatType.Float64());
    }

    /**
     * Create a float literal type from a FloatType.
     *
     * @param ftype the float type specification
     * @return a float literal type
     */
    static LiteralType float_(final FloatType ftype) {
        return new LiteralType.Float_(ftype);
    }

    /**
     * Create an int16 literal type.
     *
     * @return an int16 literal type
     */
    static LiteralType int16() {
        return integer(new IntegerType.Int16());
    }

    /**
     * Create an int32 literal type.
     *
     * @return an int32 literal type
     */
    static LiteralType int32() {
        return integer(new IntegerType.Int32());
    }

    /**
     * Create an int64 literal type.
     *
     * @return an int64 literal type
     */
    static LiteralType int64() {
        return integer(new IntegerType.Int64());
    }

    /**
     * Create an int8 literal type.
     *
     * @return an int8 literal type
     */
    static LiteralType int8() {
        return integer(new IntegerType.Int8());
    }

    /**
     * Create an integer literal type from an IntegerType.
     *
     * @param itype the integer type specification
     * @return an integer literal type
     */
    static LiteralType integer(final IntegerType itype) {
        return new LiteralType.Integer_(itype);
    }

    /**
     * Create a string literal type.
     *
     * @return a string literal type
     */
    static LiteralType string() {
        return new LiteralType.String_();
    }

    /**
     * Create a uint16 literal type.
     *
     * @return a uint16 literal type
     */
    static LiteralType uint16() {
        return integer(new IntegerType.Uint16());
    }

    /**
     * Create a uint32 literal type.
     *
     * @return a uint32 literal type
     */
    static LiteralType uint32() {
        return integer(new IntegerType.Uint32());
    }

    /**
     * Create a uint64 literal type.
     *
     * @return a uint64 literal type
     */
    static LiteralType uint64() {
        return integer(new IntegerType.Uint64());
    }

    /**
     * Create a uint8 literal type.
     *
     * @return a uint8 literal type
     */
    static LiteralType uint8() {
        return integer(new IntegerType.Uint8());
    }

    /**
     * Encode a literal type as a string.
     * @param type the literal type to encode
     * @return a string representation of the literal type
     */
    static String showLiteralType(LiteralType type) {
        return type.accept(new LiteralType.Visitor<String>() {
            @Override
            public String visit(LiteralType.Binary instance) {
                return "binary";
            }

            @Override
            public String visit(LiteralType.Boolean_ instance) {
                return "boolean";
            }

            @Override
            public String visit(LiteralType.Float_ instance) {
                return "float:" + instance.value.accept(new FloatType.Visitor<String>() {
                    @Override
                    public String visit(FloatType.Bigfloat instance) {
                        return "bigfloat";
                    }

                    @Override
                    public String visit(FloatType.Float32 instance) {
                        return "float32";
                    }

                    @Override
                    public String visit(FloatType.Float64 instance) {
                        return "float64";
                    }
                });
            }

            @Override
            public String visit(LiteralType.Integer_ instance) {
                return "integer:" + instance.value.accept(new IntegerType.Visitor<String>() {
                    @Override
                    public String visit(IntegerType.Bigint instance) {
                        return "bigint";
                    }

                    @Override
                    public String visit(IntegerType.Int8 instance) {
                        return "int8";
                    }

                    @Override
                    public String visit(IntegerType.Int16 instance) {
                        return "int16";
                    }

                    @Override
                    public String visit(IntegerType.Int32 instance) {
                        return "int32";
                    }

                    @Override
                    public String visit(IntegerType.Int64 instance) {
                        return "int64";
                    }

                    @Override
                    public String visit(IntegerType.Uint8 instance) {
                        return "uint8";
                    }

                    @Override
                    public String visit(IntegerType.Uint16 instance) {
                        return "uint16";
                    }

                    @Override
                    public String visit(IntegerType.Uint32 instance) {
                        return "uint32";
                    }

                    @Override
                    public String visit(IntegerType.Uint64 instance) {
                        return "uint64";
                    }
                });
            }

            @Override
            public String visit(LiteralType.String_ instance) {
                return "string";
            }
        });
    }
}
