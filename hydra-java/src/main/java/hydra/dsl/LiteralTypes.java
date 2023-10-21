package hydra.dsl;

import hydra.core.FloatType;
import hydra.core.IntegerType;
import hydra.core.LiteralType;


public interface LiteralTypes {
    static LiteralType bigfloat() {
        return float_(new FloatType.Bigfloat());
    }

    static LiteralType bigint() {
        return integer(new IntegerType.Bigint());
    }

    static LiteralType binary() {
        return new LiteralType.Binary();
    }

    static LiteralType boolean_() {
        return new LiteralType.Boolean_();
    }

    static LiteralType float32() {
        return float_(new FloatType.Float32());
    }

    static LiteralType float64() {
        return float_(new FloatType.Float64());
    }

    static LiteralType float_(final FloatType ftype) {
        return new LiteralType.Float_(ftype);
    }

    static LiteralType int16() {
        return integer(new IntegerType.Int16());
    }

    static LiteralType int32() {
        return integer(new IntegerType.Int32());
    }

    static LiteralType int64() {
        return integer(new IntegerType.Int64());
    }

    static LiteralType int8() {
        return integer(new IntegerType.Int8());
    }

    static LiteralType integer(final IntegerType itype) {
        return new LiteralType.Integer_(itype);
    }

    static LiteralType string() {
        return new LiteralType.String_();
    }

    static LiteralType uint16() {
        return integer(new IntegerType.Uint16());
    }

    static LiteralType uint32() {
        return integer(new IntegerType.Uint32());
    }

    static LiteralType uint64() {
        return integer(new IntegerType.Uint64());
    }

    static LiteralType uint8() {
        return integer(new IntegerType.Uint8());
    }

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
