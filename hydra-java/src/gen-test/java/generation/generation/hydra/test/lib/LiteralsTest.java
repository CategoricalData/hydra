// Note: this is an automatically generated file. Do not edit.
// hydra.lib.literals primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class LiteralsTest {

    // bigintToInt8

    @Test

    public void testBiginttoint8Positive() {

        assertEquals(

            (byte) (42),

            hydra.lib.literals.BigintToInt8.apply(new java.math.BigInteger("42")));

    }

    @Test

    public void testBiginttoint8Negative() {

        assertEquals(

            (byte) (-42),

            hydra.lib.literals.BigintToInt8.apply(new java.math.BigInteger("-42")));

    }

    // bigintToInt16

    @Test

    public void testBiginttoint16Positive() {

        assertEquals(

            (short) (1000),

            hydra.lib.literals.BigintToInt16.apply(new java.math.BigInteger("1000")));

    }

    @Test

    public void testBiginttoint16Negative() {

        assertEquals(

            (short) (-1000),

            hydra.lib.literals.BigintToInt16.apply(new java.math.BigInteger("-1000")));

    }

    // bigintToInt32

    @Test

    public void testBiginttoint32Positive() {

        assertEquals(

            42,

            hydra.lib.literals.BigintToInt32.apply(new java.math.BigInteger("42")));

    }

    @Test

    public void testBiginttoint32Negative() {

        assertEquals(

            -42,

            hydra.lib.literals.BigintToInt32.apply(new java.math.BigInteger("-42")));

    }

    @Test

    public void testBiginttoint32Zero() {

        assertEquals(

            0,

            hydra.lib.literals.BigintToInt32.apply(new java.math.BigInteger("0")));

    }

    // bigintToInt64

    @Test

    public void testBiginttoint64Positive() {

        assertEquals(

            (long) (1000000),

            hydra.lib.literals.BigintToInt64.apply(new java.math.BigInteger("1000000")));

    }

    @Test

    public void testBiginttoint64Negative() {

        assertEquals(

            (long) (-1000000),

            hydra.lib.literals.BigintToInt64.apply(new java.math.BigInteger("-1000000")));

    }

    // bigintToUint8

    @Test

    public void testBiginttouint8Zero() {

        assertEquals(

            (short) (0),

            hydra.lib.literals.BigintToUint8.apply(new java.math.BigInteger("0")));

    }

    @Test

    public void testBiginttouint8TypicalValue() {

        assertEquals(

            (short) (100),

            hydra.lib.literals.BigintToUint8.apply(new java.math.BigInteger("100")));

    }

    // bigintToUint16

    @Test

    public void testBiginttouint16Zero() {

        assertEquals(

            '\u0000',

            hydra.lib.literals.BigintToUint16.apply(new java.math.BigInteger("0")));

    }

    @Test

    public void testBiginttouint16TypicalValue() {

        assertEquals(

            '\u03E8',

            hydra.lib.literals.BigintToUint16.apply(new java.math.BigInteger("1000")));

    }

    // bigintToUint32

    @Test

    public void testBiginttouint32Zero() {

        assertEquals(

            (long) (0),

            hydra.lib.literals.BigintToUint32.apply(new java.math.BigInteger("0")));

    }

    @Test

    public void testBiginttouint32TypicalValue() {

        assertEquals(

            (long) (100000),

            hydra.lib.literals.BigintToUint32.apply(new java.math.BigInteger("100000")));

    }

    // bigintToUint64

    @Test

    public void testBiginttouint64Zero() {

        assertEquals(

            new java.math.BigInteger("0"),

            hydra.lib.literals.BigintToUint64.apply(new java.math.BigInteger("0")));

    }

    @Test

    public void testBiginttouint64TypicalValue() {

        assertEquals(

            new java.math.BigInteger("1000000"),

            hydra.lib.literals.BigintToUint64.apply(new java.math.BigInteger("1000000")));

    }

    // int8ToBigint

    @Test

    public void testInt8tobigintPositive() {

        assertEquals(

            new java.math.BigInteger("42"),

            hydra.lib.literals.Int8ToBigint.apply((byte) (42)));

    }

    @Test

    public void testInt8tobigintNegative() {

        assertEquals(

            new java.math.BigInteger("-42"),

            hydra.lib.literals.Int8ToBigint.apply((byte) (-42)));

    }

    @Test

    public void testInt8tobigintMaxValue() {

        assertEquals(

            new java.math.BigInteger("127"),

            hydra.lib.literals.Int8ToBigint.apply((byte) (127)));

    }

    @Test

    public void testInt8tobigintMinValue() {

        assertEquals(

            new java.math.BigInteger("-128"),

            hydra.lib.literals.Int8ToBigint.apply((byte) (-128)));

    }

    // int16ToBigint

    @Test

    public void testInt16tobigintPositive() {

        assertEquals(

            new java.math.BigInteger("1000"),

            hydra.lib.literals.Int16ToBigint.apply((short) (1000)));

    }

    @Test

    public void testInt16tobigintNegative() {

        assertEquals(

            new java.math.BigInteger("-1000"),

            hydra.lib.literals.Int16ToBigint.apply((short) (-1000)));

    }

    // int32ToBigint

    @Test

    public void testInt32tobigintPositive() {

        assertEquals(

            new java.math.BigInteger("42"),

            hydra.lib.literals.Int32ToBigint.apply(42));

    }

    @Test

    public void testInt32tobigintNegative() {

        assertEquals(

            new java.math.BigInteger("-42"),

            hydra.lib.literals.Int32ToBigint.apply(-42));

    }

    @Test

    public void testInt32tobigintZero() {

        assertEquals(

            new java.math.BigInteger("0"),

            hydra.lib.literals.Int32ToBigint.apply(0));

    }

    // int64ToBigint

    @Test

    public void testInt64tobigintPositive() {

        assertEquals(

            new java.math.BigInteger("1000000"),

            hydra.lib.literals.Int64ToBigint.apply((long) (1000000)));

    }

    @Test

    public void testInt64tobigintNegative() {

        assertEquals(

            new java.math.BigInteger("-1000000"),

            hydra.lib.literals.Int64ToBigint.apply((long) (-1000000)));

    }

    // uint8ToBigint

    @Test

    public void testUint8tobigintZero() {

        assertEquals(

            new java.math.BigInteger("0"),

            hydra.lib.literals.Uint8ToBigint.apply((short) (0)));

    }

    @Test

    public void testUint8tobigintMaxValue() {

        assertEquals(

            new java.math.BigInteger("255"),

            hydra.lib.literals.Uint8ToBigint.apply((short) (255)));

    }

    // uint16ToBigint

    @Test

    public void testUint16tobigintZero() {

        assertEquals(

            new java.math.BigInteger("0"),

            hydra.lib.literals.Uint16ToBigint.apply('\u0000'));

    }

    @Test

    public void testUint16tobigintTypicalValue() {

        assertEquals(

            new java.math.BigInteger("1000"),

            hydra.lib.literals.Uint16ToBigint.apply('\u03E8'));

    }

    // uint32ToBigint

    @Test

    public void testUint32tobigintZero() {

        assertEquals(

            new java.math.BigInteger("0"),

            hydra.lib.literals.Uint32ToBigint.apply((long) (0)));

    }

    @Test

    public void testUint32tobigintTypicalValue() {

        assertEquals(

            new java.math.BigInteger("100000"),

            hydra.lib.literals.Uint32ToBigint.apply((long) (100000)));

    }

    // uint64ToBigint

    @Test

    public void testUint64tobigintZero() {

        assertEquals(

            new java.math.BigInteger("0"),

            hydra.lib.literals.Uint64ToBigint.apply(new java.math.BigInteger("0")));

    }

    @Test

    public void testUint64tobigintTypicalValue() {

        assertEquals(

            new java.math.BigInteger("1000000"),

            hydra.lib.literals.Uint64ToBigint.apply(new java.math.BigInteger("1000000")));

    }

    // float32ToBigfloat

    @Test

    public void testFloat32tobigfloatPositive() {

        assertEquals(0, (new java.math.BigDecimal("2.5")).compareTo(hydra.lib.literals.Float32ToBigfloat.apply((float) (2.5))));

    }

    @Test

    public void testFloat32tobigfloatNegative() {

        assertEquals(0, (new java.math.BigDecimal("-2.5")).compareTo(hydra.lib.literals.Float32ToBigfloat.apply((float) (-2.5))));

    }

    @Test

    public void testFloat32tobigfloatZero() {

        assertEquals(0, (new java.math.BigDecimal("0.0")).compareTo(hydra.lib.literals.Float32ToBigfloat.apply((float) (0.0))));

    }

    // float64ToBigfloat

    @Test

    public void testFloat64tobigfloatPositive() {

        assertEquals(0, (new java.math.BigDecimal("3.14159")).compareTo(hydra.lib.literals.Float64ToBigfloat.apply(3.14159)));

    }

    @Test

    public void testFloat64tobigfloatNegative() {

        assertEquals(0, (new java.math.BigDecimal("-2.71828")).compareTo(hydra.lib.literals.Float64ToBigfloat.apply(-2.71828)));

    }

    @Test

    public void testFloat64tobigfloatZero() {

        assertEquals(0, (new java.math.BigDecimal("0.0")).compareTo(hydra.lib.literals.Float64ToBigfloat.apply(0.0)));

    }

    // bigfloatToFloat32

    @Test

    public void testBigfloattofloat32Positive() {

        assertEquals(

            (float) (3.140000104904175),

            hydra.lib.literals.BigfloatToFloat32.apply(new java.math.BigDecimal("3.14")),

            1e-15);

    }

    @Test

    public void testBigfloattofloat32Negative() {

        assertEquals(

            (float) (-2.5),

            hydra.lib.literals.BigfloatToFloat32.apply(new java.math.BigDecimal("-2.5")),

            1e-15);

    }

    @Test

    public void testBigfloattofloat32Zero() {

        assertEquals(

            (float) (0.0),

            hydra.lib.literals.BigfloatToFloat32.apply(new java.math.BigDecimal("0.0")),

            1e-15);

    }

    // bigfloatToFloat64

    @Test

    public void testBigfloattofloat64Positive() {

        assertEquals(

            3.14159,

            hydra.lib.literals.BigfloatToFloat64.apply(new java.math.BigDecimal("3.14159")),

            1e-15);

    }

    @Test

    public void testBigfloattofloat64Negative() {

        assertEquals(

            -2.71828,

            hydra.lib.literals.BigfloatToFloat64.apply(new java.math.BigDecimal("-2.71828")),

            1e-15);

    }

    @Test

    public void testBigfloattofloat64Zero() {

        assertEquals(

            0.0,

            hydra.lib.literals.BigfloatToFloat64.apply(new java.math.BigDecimal("0.0")),

            1e-15);

    }

    // bigintToBigfloat

    @Test

    public void testBiginttobigfloatPositive() {

        assertEquals(0, (new java.math.BigDecimal("42.0")).compareTo(hydra.lib.literals.BigintToBigfloat.apply(new java.math.BigInteger("42"))));

    }

    @Test

    public void testBiginttobigfloatNegative() {

        assertEquals(0, (new java.math.BigDecimal("-42.0")).compareTo(hydra.lib.literals.BigintToBigfloat.apply(new java.math.BigInteger("-42"))));

    }

    @Test

    public void testBiginttobigfloatZero() {

        assertEquals(0, (new java.math.BigDecimal("0.0")).compareTo(hydra.lib.literals.BigintToBigfloat.apply(new java.math.BigInteger("0"))));

    }

    // bigfloatToBigint

    @Test

    public void testBigfloattobigintPositive() {

        assertEquals(

            new java.math.BigInteger("43"),

            hydra.lib.literals.BigfloatToBigint.apply(new java.math.BigDecimal("42.7")));

    }

    @Test

    public void testBigfloattobigintNegative() {

        assertEquals(

            new java.math.BigInteger("-43"),

            hydra.lib.literals.BigfloatToBigint.apply(new java.math.BigDecimal("-42.7")));

    }

    @Test

    public void testBigfloattobigintZero() {

        assertEquals(

            new java.math.BigInteger("0"),

            hydra.lib.literals.BigfloatToBigint.apply(new java.math.BigDecimal("0.0")));

    }

    @Test

    public void testBigfloattobigintRoundDown() {

        assertEquals(

            new java.math.BigInteger("42"),

            hydra.lib.literals.BigfloatToBigint.apply(new java.math.BigDecimal("42.3")));

    }

    @Test

    public void testBigfloattobigintHalfEvenUp() {

        assertEquals(

            new java.math.BigInteger("42"),

            hydra.lib.literals.BigfloatToBigint.apply(new java.math.BigDecimal("42.5")));

    }

    @Test

    public void testBigfloattobigintHalfEvenDown() {

        assertEquals(

            new java.math.BigInteger("44"),

            hydra.lib.literals.BigfloatToBigint.apply(new java.math.BigDecimal("43.5")));

    }

    // showInt8

    @Test

    public void testShowint8Positive() {

        assertEquals(

            "42",

            hydra.lib.literals.ShowInt8.apply((byte) (42)));

    }

    @Test

    public void testShowint8Negative() {

        assertEquals(

            "-42",

            hydra.lib.literals.ShowInt8.apply((byte) (-42)));

    }

    // showInt16

    @Test

    public void testShowint16Positive() {

        assertEquals(

            "1000",

            hydra.lib.literals.ShowInt16.apply((short) (1000)));

    }

    @Test

    public void testShowint16Negative() {

        assertEquals(

            "-1000",

            hydra.lib.literals.ShowInt16.apply((short) (-1000)));

    }

    // showInt32

    @Test

    public void testShowint32Positive() {

        assertEquals(

            "42",

            hydra.lib.literals.ShowInt32.apply(42));

    }

    @Test

    public void testShowint32Negative() {

        assertEquals(

            "-42",

            hydra.lib.literals.ShowInt32.apply(-42));

    }

    @Test

    public void testShowint32Zero() {

        assertEquals(

            "0",

            hydra.lib.literals.ShowInt32.apply(0));

    }

    // showInt64

    @Test

    public void testShowint64Positive() {

        assertEquals(

            "1000000",

            hydra.lib.literals.ShowInt64.apply((long) (1000000)));

    }

    @Test

    public void testShowint64Negative() {

        assertEquals(

            "-1000000",

            hydra.lib.literals.ShowInt64.apply((long) (-1000000)));

    }

    // showUint8

    @Test

    public void testShowuint8Zero() {

        assertEquals(

            "0",

            hydra.lib.literals.ShowUint8.apply((short) (0)));

    }

    @Test

    public void testShowuint8MaxValue() {

        assertEquals(

            "255",

            hydra.lib.literals.ShowUint8.apply((short) (255)));

    }

    // showUint16

    @Test

    public void testShowuint16Zero() {

        assertEquals(

            "0",

            hydra.lib.literals.ShowUint16.apply('\u0000'));

    }

    @Test

    public void testShowuint16TypicalValue() {

        assertEquals(

            "1000",

            hydra.lib.literals.ShowUint16.apply('\u03E8'));

    }

    // showUint32

    @Test

    public void testShowuint32Zero() {

        assertEquals(

            "0",

            hydra.lib.literals.ShowUint32.apply((long) (0)));

    }

    @Test

    public void testShowuint32TypicalValue() {

        assertEquals(

            "100000",

            hydra.lib.literals.ShowUint32.apply((long) (100000)));

    }

    // showUint64

    @Test

    public void testShowuint64Zero() {

        assertEquals(

            "0",

            hydra.lib.literals.ShowUint64.apply(new java.math.BigInteger("0")));

    }

    @Test

    public void testShowuint64TypicalValue() {

        assertEquals(

            "1000000",

            hydra.lib.literals.ShowUint64.apply(new java.math.BigInteger("1000000")));

    }

    // showBigint

    @Test

    public void testShowbigintPositive() {

        assertEquals(

            "42",

            hydra.lib.literals.ShowBigint.apply(new java.math.BigInteger("42")));

    }

    @Test

    public void testShowbigintNegative() {

        assertEquals(

            "-42",

            hydra.lib.literals.ShowBigint.apply(new java.math.BigInteger("-42")));

    }

    @Test

    public void testShowbigintZero() {

        assertEquals(

            "0",

            hydra.lib.literals.ShowBigint.apply(new java.math.BigInteger("0")));

    }

    // showFloat32

    @Test

    public void testShowfloat32Positive() {

        assertEquals(

            "3.14",

            hydra.lib.literals.ShowFloat32.apply((float) (3.140000104904175)));

    }

    @Test

    public void testShowfloat32Negative() {

        assertEquals(

            "-2.5",

            hydra.lib.literals.ShowFloat32.apply((float) (-2.5)));

    }

    @Test

    public void testShowfloat32Zero() {

        assertEquals(

            "0.0",

            hydra.lib.literals.ShowFloat32.apply((float) (0.0)));

    }

    @Test

    public void testShowfloat32SmallPositive() {

        assertEquals(

            "5.0e-2",

            hydra.lib.literals.ShowFloat32.apply((float) (5.000000074505806e-2)));

    }

    @Test

    public void testShowfloat32SmallPositive2() {

        assertEquals(

            "3.0e-2",

            hydra.lib.literals.ShowFloat32.apply((float) (2.9999999329447746e-2)));

    }

    @Test

    public void testShowfloat32VerySmall() {

        assertEquals(

            "1.0e-3",

            hydra.lib.literals.ShowFloat32.apply((float) (1.0000000474974513e-3)));

    }

    @Test

    public void testShowfloat32NormalDecimal() {

        assertEquals(

            "0.1",

            hydra.lib.literals.ShowFloat32.apply((float) (0.10000000149011612)));

    }

    // showFloat64

    @Test

    public void testShowfloat64Positive() {

        assertEquals(

            "3.14159",

            hydra.lib.literals.ShowFloat64.apply(3.14159));

    }

    @Test

    public void testShowfloat64Zero() {

        assertEquals(

            "0.0",

            hydra.lib.literals.ShowFloat64.apply(0.0));

    }

    @Test

    public void testShowfloat64SmallPositive() {

        assertEquals(

            "5.0e-2",

            hydra.lib.literals.ShowFloat64.apply(5.0e-2));

    }

    @Test

    public void testShowfloat64SmallPositive2() {

        assertEquals(

            "3.0e-2",

            hydra.lib.literals.ShowFloat64.apply(3.0e-2));

    }

    @Test

    public void testShowfloat64VerySmall() {

        assertEquals(

            "1.0e-3",

            hydra.lib.literals.ShowFloat64.apply(1.0e-3));

    }

    @Test

    public void testShowfloat64NormalDecimal() {

        assertEquals(

            "0.1",

            hydra.lib.literals.ShowFloat64.apply(0.1));

    }

    // showBigfloat

    @Test

    public void testShowbigfloatPositive() {

        assertEquals(

            "3.14",

            hydra.lib.literals.ShowBigfloat.apply(new java.math.BigDecimal("3.14")));

    }

    @Test

    public void testShowbigfloatZero() {

        assertEquals(

            "0.0",

            hydra.lib.literals.ShowBigfloat.apply(new java.math.BigDecimal("0.0")));

    }

    @Test

    public void testShowbigfloatSmallPositive() {

        assertEquals(

            "5.0e-2",

            hydra.lib.literals.ShowBigfloat.apply(new java.math.BigDecimal("5.0e-2")));

    }

    @Test

    public void testShowbigfloatSmallPositive2() {

        assertEquals(

            "3.0e-2",

            hydra.lib.literals.ShowBigfloat.apply(new java.math.BigDecimal("3.0e-2")));

    }

    @Test

    public void testShowbigfloatVerySmall() {

        assertEquals(

            "1.0e-3",

            hydra.lib.literals.ShowBigfloat.apply(new java.math.BigDecimal("1.0e-3")));

    }

    @Test

    public void testShowbigfloatNormalDecimal() {

        assertEquals(

            "0.1",

            hydra.lib.literals.ShowBigfloat.apply(new java.math.BigDecimal("0.1")));

    }

    // showBoolean

    @Test

    public void testShowbooleanTrue() {

        assertEquals(

            "true",

            hydra.lib.literals.ShowBoolean.apply(true));

    }

    @Test

    public void testShowbooleanFalse() {

        assertEquals(

            "false",

            hydra.lib.literals.ShowBoolean.apply(false));

    }

    // showString

    @Test

    public void testShowstringSimple() {

        assertEquals(

            "\"hello\"",

            hydra.lib.literals.ShowString.apply("hello"));

    }

    @Test

    public void testShowstringEmpty() {

        assertEquals(

            "\"\"",

            hydra.lib.literals.ShowString.apply(""));

    }

    // readInt8

    @Test

    public void testReadint8Positive() {

        assertEquals(

            hydra.util.Maybe.just((byte) (42)),

            hydra.lib.literals.ReadInt8.apply("42"));

    }

    @Test

    public void testReadint8Negative() {

        assertEquals(

            hydra.util.Maybe.just((byte) (-42)),

            hydra.lib.literals.ReadInt8.apply("-42"));

    }

    @Test

    public void testReadint8MaxValue() {

        assertEquals(

            hydra.util.Maybe.just((byte) (127)),

            hydra.lib.literals.ReadInt8.apply("127"));

    }

    @Test

    public void testReadint8MinValue() {

        assertEquals(

            hydra.util.Maybe.just((byte) (-128)),

            hydra.lib.literals.ReadInt8.apply("-128"));

    }

    @Test

    public void testReadint8Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadInt8.apply("abc"));

    }

    @Test

    public void testReadint8Overflow() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadInt8.apply("128"));

    }

    // readInt16

    @Test

    public void testReadint16Positive() {

        assertEquals(

            hydra.util.Maybe.just((short) (1000)),

            hydra.lib.literals.ReadInt16.apply("1000"));

    }

    @Test

    public void testReadint16Negative() {

        assertEquals(

            hydra.util.Maybe.just((short) (-1000)),

            hydra.lib.literals.ReadInt16.apply("-1000"));

    }

    @Test

    public void testReadint16Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadInt16.apply("abc"));

    }

    // readInt32

    @Test

    public void testReadint32Positive() {

        assertEquals(

            hydra.util.Maybe.just(42),

            hydra.lib.literals.ReadInt32.apply("42"));

    }

    @Test

    public void testReadint32Negative() {

        assertEquals(

            hydra.util.Maybe.just(-42),

            hydra.lib.literals.ReadInt32.apply("-42"));

    }

    @Test

    public void testReadint32Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadInt32.apply("abc"));

    }

    // readInt64

    @Test

    public void testReadint64Positive() {

        assertEquals(

            hydra.util.Maybe.just((long) (1000000)),

            hydra.lib.literals.ReadInt64.apply("1000000"));

    }

    @Test

    public void testReadint64Negative() {

        assertEquals(

            hydra.util.Maybe.just((long) (-1000000)),

            hydra.lib.literals.ReadInt64.apply("-1000000"));

    }

    @Test

    public void testReadint64Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadInt64.apply("abc"));

    }

    // readUint8

    @Test

    public void testReaduint8Zero() {

        assertEquals(

            hydra.util.Maybe.just((short) (0)),

            hydra.lib.literals.ReadUint8.apply("0"));

    }

    @Test

    public void testReaduint8Typical() {

        assertEquals(

            hydra.util.Maybe.just((short) (100)),

            hydra.lib.literals.ReadUint8.apply("100"));

    }

    @Test

    public void testReaduint8MaxValue() {

        assertEquals(

            hydra.util.Maybe.just((short) (255)),

            hydra.lib.literals.ReadUint8.apply("255"));

    }

    @Test

    public void testReaduint8Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadUint8.apply("abc"));

    }

    @Test

    public void testReaduint8Negative() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadUint8.apply("-1"));

    }

    // readUint16

    @Test

    public void testReaduint16Zero() {

        assertEquals(

            hydra.util.Maybe.just('\u0000'),

            hydra.lib.literals.ReadUint16.apply("0"));

    }

    @Test

    public void testReaduint16Typical() {

        assertEquals(

            hydra.util.Maybe.just('\u03E8'),

            hydra.lib.literals.ReadUint16.apply("1000"));

    }

    @Test

    public void testReaduint16Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadUint16.apply("abc"));

    }

    @Test

    public void testReaduint16Negative() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadUint16.apply("-1"));

    }

    // readUint32

    @Test

    public void testReaduint32Zero() {

        assertEquals(

            hydra.util.Maybe.just((long) (0)),

            hydra.lib.literals.ReadUint32.apply("0"));

    }

    @Test

    public void testReaduint32Typical() {

        assertEquals(

            hydra.util.Maybe.just((long) (100000)),

            hydra.lib.literals.ReadUint32.apply("100000"));

    }

    @Test

    public void testReaduint32Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadUint32.apply("abc"));

    }

    @Test

    public void testReaduint32Negative() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadUint32.apply("-1"));

    }

    // readUint64

    @Test

    public void testReaduint64Zero() {

        assertEquals(

            hydra.util.Maybe.just(new java.math.BigInteger("0")),

            hydra.lib.literals.ReadUint64.apply("0"));

    }

    @Test

    public void testReaduint64Typical() {

        assertEquals(

            hydra.util.Maybe.just(new java.math.BigInteger("1000000")),

            hydra.lib.literals.ReadUint64.apply("1000000"));

    }

    @Test

    public void testReaduint64Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadUint64.apply("abc"));

    }

    @Test

    public void testReaduint64Negative() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadUint64.apply("-1"));

    }

    // readBigint

    @Test

    public void testReadbigintPositive() {

        assertEquals(

            hydra.util.Maybe.just(new java.math.BigInteger("42")),

            hydra.lib.literals.ReadBigint.apply("42"));

    }

    @Test

    public void testReadbigintNegative() {

        assertEquals(

            hydra.util.Maybe.just(new java.math.BigInteger("-42")),

            hydra.lib.literals.ReadBigint.apply("-42"));

    }

    @Test

    public void testReadbigintZero() {

        assertEquals(

            hydra.util.Maybe.just(new java.math.BigInteger("0")),

            hydra.lib.literals.ReadBigint.apply("0"));

    }

    @Test

    public void testReadbigintLarge() {

        assertEquals(

            hydra.util.Maybe.just(new java.math.BigInteger("123456789012345678901234567890")),

            hydra.lib.literals.ReadBigint.apply("123456789012345678901234567890"));

    }

    @Test

    public void testReadbigintInvalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadBigint.apply("abc"));

    }

    // readFloat32

    @Test

    public void testReadfloat32Positive() {

        assertEquals(

            hydra.util.Maybe.just((float) (3.140000104904175)),

            hydra.lib.literals.ReadFloat32.apply("3.14"));

    }

    @Test

    public void testReadfloat32Negative() {

        assertEquals(

            hydra.util.Maybe.just((float) (-2.5)),

            hydra.lib.literals.ReadFloat32.apply("-2.5"));

    }

    @Test

    public void testReadfloat32Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadFloat32.apply("abc"));

    }

    // readFloat64

    @Test

    public void testReadfloat64Positive() {

        assertEquals(

            hydra.util.Maybe.just(3.14159),

            hydra.lib.literals.ReadFloat64.apply("3.14159"));

    }

    @Test

    public void testReadfloat64Negative() {

        assertEquals(

            hydra.util.Maybe.just(-2.71828),

            hydra.lib.literals.ReadFloat64.apply("-2.71828"));

    }

    @Test

    public void testReadfloat64Invalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadFloat64.apply("abc"));

    }

    // readBigfloat

    @Test

    public void testReadbigfloatPositive() {

        assertEquals(

            hydra.util.Maybe.just(new java.math.BigDecimal("3.14")),

            hydra.lib.literals.ReadBigfloat.apply("3.14"));

    }

    @Test

    public void testReadbigfloatInvalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadBigfloat.apply("abc"));

    }

    // readBoolean

    @Test

    public void testReadbooleanTrue() {

        assertEquals(

            hydra.util.Maybe.just(true),

            hydra.lib.literals.ReadBoolean.apply("true"));

    }

    @Test

    public void testReadbooleanFalse() {

        assertEquals(

            hydra.util.Maybe.just(false),

            hydra.lib.literals.ReadBoolean.apply("false"));

    }

    @Test

    public void testReadbooleanInvalid() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadBoolean.apply("yes"));

    }

    // readString

    @Test

    public void testReadstringQuotedString() {

        assertEquals(

            hydra.util.Maybe.just("hello"),

            hydra.lib.literals.ReadString.apply("\"hello\""));

    }

    @Test

    public void testReadstringEmptyQuoted() {

        assertEquals(

            hydra.util.Maybe.just(""),

            hydra.lib.literals.ReadString.apply("\"\""));

    }

    @Test

    public void testReadstringUnquoted() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.literals.ReadString.apply("hello"));

    }

    // stringToBinary

    @Test

    public void testStringtobinarySimpleBase64() {

        assertArrayEquals(

            new byte[] {104, 101, 108, 108, 111},

            hydra.lib.literals.StringToBinary.apply("aGVsbG8="));

    }

    @Test

    public void testStringtobinaryEmptyString() {

        assertArrayEquals(

            new byte[] {},

            hydra.lib.literals.StringToBinary.apply(""));

    }

    // binaryToString

    @Test

    public void testBinarytostringSimpleBinary() {

        assertEquals(

            "aGVsbG8=",

            hydra.lib.literals.BinaryToString.apply(new byte[] {104, 101, 108, 108, 111}));

    }

    @Test

    public void testBinarytostringEmptyBinary() {

        assertEquals(

            "",

            hydra.lib.literals.BinaryToString.apply(new byte[] {}));

    }
}
