package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.literals.BigfloatToBigint;
import hydra.lib.literals.BigfloatToFloat32;
import hydra.lib.literals.BigfloatToFloat64;
import hydra.lib.literals.BigintToBigfloat;
import hydra.lib.literals.BigintToInt16;
import hydra.lib.literals.BigintToInt32;
import hydra.lib.literals.BigintToInt64;
import hydra.lib.literals.BigintToInt8;
import hydra.lib.literals.BigintToUint16;
import hydra.lib.literals.BigintToUint32;
import hydra.lib.literals.BigintToUint64;
import hydra.lib.literals.BigintToUint8;
import hydra.lib.literals.BinaryToString;
import hydra.lib.literals.Float32ToBigfloat;
import hydra.lib.literals.Float64ToBigfloat;
import hydra.lib.literals.Int16ToBigint;
import hydra.lib.literals.Int32ToBigint;
import hydra.lib.literals.Int64ToBigint;
import hydra.lib.literals.Int8ToBigint;
import hydra.lib.literals.ReadBigfloat;
import hydra.lib.literals.ReadBoolean;
import hydra.lib.literals.ReadFloat32;
import hydra.lib.literals.ReadFloat64;
import hydra.lib.literals.ReadInt32;
import hydra.lib.literals.ReadInt64;
import hydra.lib.literals.ReadString;
import hydra.lib.literals.ShowBigfloat;
import hydra.lib.literals.ShowBigint;
import hydra.lib.literals.ShowBoolean;
import hydra.lib.literals.ShowFloat32;
import hydra.lib.literals.ShowFloat64;
import hydra.lib.literals.ShowInt16;
import hydra.lib.literals.ShowInt32;
import hydra.lib.literals.ShowInt64;
import hydra.lib.literals.ShowInt8;
import hydra.lib.literals.ShowString;
import hydra.lib.literals.ShowUint16;
import hydra.lib.literals.ShowUint32;
import hydra.lib.literals.ShowUint64;
import hydra.lib.literals.ShowUint8;
import hydra.lib.literals.StringToBinary;
import hydra.lib.literals.Uint16ToBigint;
import hydra.lib.literals.Uint32ToBigint;
import hydra.lib.literals.Uint64ToBigint;
import hydra.lib.literals.Uint8ToBigint;

/**
 * DSL interface providing literal primitive operations for type conversions and parsing/showing values.
 */
public interface Literals {
    /**
     * Returns a term representing the bigfloat-to-bigint conversion primitive.
     *
     * @return a term for converting a bigfloat to a bigint
     */
    static Term bigfloatToBigint() {
        return new BigfloatToBigint().term();
    }

    /**
     * Returns a term representing the bigfloat-to-float32 conversion primitive.
     *
     * @return a term for converting a bigfloat to a float32
     */
    static Term bigfloatToFloat32() {
        return new BigfloatToFloat32().term();
    }

    /**
     * Returns a term representing the bigfloat-to-float64 conversion primitive.
     *
     * @return a term for converting a bigfloat to a float64
     */
    static Term bigfloatToFloat64() {
        return new BigfloatToFloat64().term();
    }

    /**
     * Returns a term representing the bigint-to-bigfloat conversion primitive.
     *
     * @return a term for converting a bigint to a bigfloat
     */
    static Term bigintToBigfloat() {
        return new BigintToBigfloat().term();
    }

    /**
     * Returns a term representing the bigint-to-int8 conversion primitive.
     *
     * @return a term for converting a bigint to an int8
     */
    static Term bigintToInt8() {
        return new BigintToInt8().term();
    }

    /**
     * Returns a term representing the bigint-to-int16 conversion primitive.
     *
     * @return a term for converting a bigint to an int16
     */
    static Term bigintToInt16() {
        return new BigintToInt16().term();
    }

    /**
     * Returns a term representing the bigint-to-int32 conversion primitive.
     *
     * @return a term for converting a bigint to an int32
     */
    static Term bigintToInt32() {
        return new BigintToInt32().term();
    }

    /**
     * Returns a term representing the bigint-to-int64 conversion primitive.
     *
     * @return a term for converting a bigint to an int64
     */
    static Term bigintToInt64() {
        return new BigintToInt64().term();
    }

    /**
     * Returns a term representing the bigint-to-uint8 conversion primitive.
     *
     * @return a term for converting a bigint to a uint8
     */
    static Term bigintToUint8() {
        return new BigintToUint8().term();
    }

    /**
     * Returns a term representing the bigint-to-uint16 conversion primitive.
     *
     * @return a term for converting a bigint to a uint16
     */
    static Term bigintToUint16() {
        return new BigintToUint16().term();
    }

    /**
     * Returns a term representing the bigint-to-uint32 conversion primitive.
     *
     * @return a term for converting a bigint to a uint32
     */
    static Term bigintToUint32() {
        return new BigintToUint32().term();
    }

    /**
     * Returns a term representing the bigint-to-uint64 conversion primitive.
     *
     * @return a term for converting a bigint to a uint64
     */
    static Term bigintToUint64() {
        return new BigintToUint64().term();
    }

    /**
     * Returns a term representing the binary-to-string conversion primitive.
     *
     * @return a term for converting binary data to a string
     */
    static Term binaryToString() {
        return new BinaryToString().term();
    }

    /**
     * Returns a term representing the float32-to-bigfloat conversion primitive.
     *
     * @return a term for converting a float32 to a bigfloat
     */
    static Term float32ToBigfloat() {
        return new Float32ToBigfloat().term();
    }

    /**
     * Returns a term representing the float64-to-bigfloat conversion primitive.
     *
     * @return a term for converting a float64 to a bigfloat
     */
    static Term float64ToBigfloat() {
        return new Float64ToBigfloat().term();
    }

    /**
     * Returns a term representing the int8-to-bigint conversion primitive.
     *
     * @return a term for converting an int8 to a bigint
     */
    static Term int8ToBigint() {
        return new Int8ToBigint().term();
    }

    /**
     * Returns a term representing the int16-to-bigint conversion primitive.
     *
     * @return a term for converting an int16 to a bigint
     */
    static Term int16ToBigint() {
        return new Int16ToBigint().term();
    }

    /**
     * Returns a term representing the int32-to-bigint conversion primitive.
     *
     * @return a term for converting an int32 to a bigint
     */
    static Term int32ToBigint() {
        return new Int32ToBigint().term();
    }

    /**
     * Returns a term representing the int64-to-bigint conversion primitive.
     *
     * @return a term for converting an int64 to a bigint
     */
    static Term int64ToBigint() {
        return new Int64ToBigint().term();
    }

    /**
     * Returns a term representing the read-bigfloat primitive.
     *
     * @return a term for parsing a string to a bigfloat
     */
    static Term readBigfloat() {
        return new ReadBigfloat().term();
    }

    /**
     * Returns a term representing the read-boolean primitive.
     *
     * @return a term for parsing a string to a boolean
     */
    static Term readBoolean() {
        return new ReadBoolean().term();
    }

    /**
     * Returns a term representing the read-float32 primitive.
     *
     * @return a term for parsing a string to a float32
     */
    static Term readFloat32() {
        return new ReadFloat32().term();
    }

    /**
     * Returns a term representing the read-float64 primitive.
     *
     * @return a term for parsing a string to a float64
     */
    static Term readFloat64() {
        return new ReadFloat64().term();
    }

    /**
     * Returns a term representing the read-int32 primitive.
     *
     * @return a term for parsing a string to an int32
     */
    static Term readInt32() {
        return new ReadInt32().term();
    }

    /**
     * Returns a term representing the read-int64 primitive.
     *
     * @return a term for parsing a string to an int64
     */
    static Term readInt64() {
        return new ReadInt64().term();
    }

    /**
     * Returns a term representing the read-string primitive.
     *
     * @return a term for parsing a string (identity operation)
     */
    static Term readString() {
        return new ReadString().term();
    }

    /**
     * Returns a term representing the show-bigfloat primitive.
     *
     * @return a term for converting a bigfloat to a string
     */
    static Term showBigfloat() {
        return new ShowBigfloat().term();
    }

    /**
     * Returns a term representing the show-bigint primitive.
     *
     * @return a term for converting a bigint to a string
     */
    static Term showBigint() {
        return new ShowBigint().term();
    }

    /**
     * Returns a term representing the show-boolean primitive.
     *
     * @return a term for converting a boolean to a string
     */
    static Term showBoolean() {
        return new ShowBoolean().term();
    }

    /**
     * Returns a term representing the show-float32 primitive.
     *
     * @return a term for converting a float32 to a string
     */
    static Term showFloat32() {
        return new ShowFloat32().term();
    }

    /**
     * Returns a term representing the show-float64 primitive.
     *
     * @return a term for converting a float64 to a string
     */
    static Term showFloat64() {
        return new ShowFloat64().term();
    }

    /**
     * Returns a term representing the show-int8 primitive.
     *
     * @return a term for converting an int8 to a string
     */
    static Term showInt8() {
        return new ShowInt8().term();
    }

    /**
     * Returns a term representing the show-int16 primitive.
     *
     * @return a term for converting an int16 to a string
     */
    static Term showInt16() {
        return new ShowInt16().term();
    }

    /**
     * Returns a term representing the show-int32 primitive.
     *
     * @return a term for converting an int32 to a string
     */
    static Term showInt32() {
        return new ShowInt32().term();
    }

    /**
     * Returns a term representing the show-int64 primitive.
     *
     * @return a term for converting an int64 to a string
     */
    static Term showInt64() {
        return new ShowInt64().term();
    }

    /**
     * Returns a term representing the show-uint8 primitive.
     *
     * @return a term for converting a uint8 to a string
     */
    static Term showUint8() {
        return new ShowUint8().term();
    }

    /**
     * Returns a term representing the show-uint16 primitive.
     *
     * @return a term for converting a uint16 to a string
     */
    static Term showUint16() {
        return new ShowUint16().term();
    }

    /**
     * Returns a term representing the show-uint32 primitive.
     *
     * @return a term for converting a uint32 to a string
     */
    static Term showUint32() {
        return new ShowUint32().term();
    }

    /**
     * Returns a term representing the show-uint64 primitive.
     *
     * @return a term for converting a uint64 to a string
     */
    static Term showUint64() {
        return new ShowUint64().term();
    }

    /**
     * Returns a term representing the show-string primitive.
     *
     * @return a term for converting a string to a string (identity operation)
     */
    static Term showString() {
        return new ShowString().term();
    }

    /**
     * Returns a term representing the string-to-binary conversion primitive.
     *
     * @return a term for converting a string to binary data
     */
    static Term stringToBinary() {
        return new StringToBinary().term();
    }

    /**
     * Returns a term representing the uint8-to-bigint conversion primitive.
     *
     * @return a term for converting a uint8 to a bigint
     */
    static Term uint8ToBigint() {
        return new Uint8ToBigint().term();
    }

    /**
     * Returns a term representing the uint16-to-bigint conversion primitive.
     *
     * @return a term for converting a uint16 to a bigint
     */
    static Term uint16ToBigint() {
        return new Uint16ToBigint().term();
    }

    /**
     * Returns a term representing the uint32-to-bigint conversion primitive.
     *
     * @return a term for converting a uint32 to a bigint
     */
    static Term uint32ToBigint() {
        return new Uint32ToBigint().term();
    }

    /**
     * Returns a term representing the uint64-to-bigint conversion primitive.
     *
     * @return a term for converting a uint64 to a bigint
     */
    static Term uint64ToBigint() {
        return new Uint64ToBigint().term();
    }
}
