package hydra.dsl;

import hydra.core.FloatValue;
import hydra.core.IntegerValue;
import hydra.core.Literal;

import java.math.BigInteger;


/**
 * DSL functions for constructing literal values
 */
public interface Literals {

    static Literal bigfloat(final double value) {
        return float_(new FloatValue.Bigfloat(value));
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
    
    static Literal int8(final short value) {
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

    static Literal uint8(final byte value) {
        return integer(new IntegerValue.Uint8(value));
    }

    static Literal uint16(final char value) {
        return integer(new IntegerValue.Uint16(value));
    }

    static Literal uint32(final long value) {
        return integer(new IntegerValue.Uint32(value));
    }

    static Literal uint64(final BigInteger value) {
        return integer(new IntegerValue.Uint64(value));
    }
}
