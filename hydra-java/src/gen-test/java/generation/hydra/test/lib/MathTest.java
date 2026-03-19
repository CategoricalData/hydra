// Note: this is an automatically generated file. Do not edit.
// hydra.lib.math primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class MathTest {

    // abs

    @Test

    public void testAbsPositive() {

        assertEquals(

            5,

            hydra.lib.math.Abs.apply(5));

    }

    @Test

    public void testAbsNegative() {

        assertEquals(

            5,

            hydra.lib.math.Abs.apply(-5));

    }

    @Test

    public void testAbsZero() {

        assertEquals(

            0,

            hydra.lib.math.Abs.apply(0));

    }

    // add

    @Test

    public void testAddPositiveNumbers() {

        assertEquals(

            8,

            hydra.lib.math.Add.apply(
  3,
  5));

    }

    @Test

    public void testAddNegativeNumbers() {

        assertEquals(

            -8,

            hydra.lib.math.Add.apply(
  -3,
  -5));

    }

    @Test

    public void testAddMixedSign() {

        assertEquals(

            7,

            hydra.lib.math.Add.apply(
  10,
  -3));

    }

    @Test

    public void testAddWithZero() {

        assertEquals(

            42,

            hydra.lib.math.Add.apply(
  42,
  0));

    }

    // div

    @Test

    public void testDivExactDivision() {

        assertEquals(

            5,

            hydra.lib.math.Div.apply(
  10,
  2));

    }

    @Test

    public void testDivTruncatesTowardNegativeInfinity() {

        assertEquals(

            3,

            hydra.lib.math.Div.apply(
  10,
  3));

    }

    @Test

    public void testDivNegativeDividend() {

        assertEquals(

            -4,

            hydra.lib.math.Div.apply(
  -10,
  3));

    }

    @Test

    public void testDivNegativeDivisor() {

        assertEquals(

            -4,

            hydra.lib.math.Div.apply(
  10,
  -3));

    }

    // even

    @Test

    public void testEvenEvenPositive() {

        assertEquals(

            true,

            hydra.lib.math.Even.apply(4));

    }

    @Test

    public void testEvenOddPositive() {

        assertEquals(

            false,

            hydra.lib.math.Even.apply(5));

    }

    @Test

    public void testEvenEvenNegative() {

        assertEquals(

            true,

            hydra.lib.math.Even.apply(-4));

    }

    @Test

    public void testEvenOddNegative() {

        assertEquals(

            false,

            hydra.lib.math.Even.apply(-5));

    }

    @Test

    public void testEvenZero() {

        assertEquals(

            true,

            hydra.lib.math.Even.apply(0));

    }

    // max

    @Test

    public void testMaxFirstIsLarger() {

        assertEquals(

            10,

            hydra.lib.math.Max.apply(
  10,
  5));

    }

    @Test

    public void testMaxSecondIsLarger() {

        assertEquals(

            10,

            hydra.lib.math.Max.apply(
  5,
  10));

    }

    @Test

    public void testMaxEqualValues() {

        assertEquals(

            7,

            hydra.lib.math.Max.apply(
  7,
  7));

    }

    @Test

    public void testMaxNegativeNumbers() {

        assertEquals(

            -3,

            hydra.lib.math.Max.apply(
  -3,
  -5));

    }

    @Test

    public void testMaxMixedSign() {

        assertEquals(

            5,

            hydra.lib.math.Max.apply(
  -5,
  5));

    }

    @Test

    public void testMaxWithZero() {

        assertEquals(

            42,

            hydra.lib.math.Max.apply(
  0,
  42));

    }

    // min

    @Test

    public void testMinFirstIsSmaller() {

        assertEquals(

            5,

            hydra.lib.math.Min.apply(
  5,
  10));

    }

    @Test

    public void testMinSecondIsSmaller() {

        assertEquals(

            5,

            hydra.lib.math.Min.apply(
  10,
  5));

    }

    @Test

    public void testMinEqualValues() {

        assertEquals(

            7,

            hydra.lib.math.Min.apply(
  7,
  7));

    }

    @Test

    public void testMinNegativeNumbers() {

        assertEquals(

            -5,

            hydra.lib.math.Min.apply(
  -3,
  -5));

    }

    @Test

    public void testMinMixedSign() {

        assertEquals(

            -5,

            hydra.lib.math.Min.apply(
  -5,
  5));

    }

    @Test

    public void testMinWithZero() {

        assertEquals(

            0,

            hydra.lib.math.Min.apply(
  0,
  42));

    }

    // mod

    @Test

    public void testModBasicModulo() {

        assertEquals(

            1,

            hydra.lib.math.Mod.apply(
  10,
  3));

    }

    @Test

    public void testModExactDivision() {

        assertEquals(

            0,

            hydra.lib.math.Mod.apply(
  10,
  2));

    }

    @Test

    public void testModNegativeDividend() {

        assertEquals(

            2,

            hydra.lib.math.Mod.apply(
  -10,
  3));

    }

    @Test

    public void testModNegativeDivisor() {

        assertEquals(

            -2,

            hydra.lib.math.Mod.apply(
  10,
  -3));

    }

    // mul

    @Test

    public void testMulPositiveNumbers() {

        assertEquals(

            15,

            hydra.lib.math.Mul.apply(
  3,
  5));

    }

    @Test

    public void testMulNegativeNumbers() {

        assertEquals(

            15,

            hydra.lib.math.Mul.apply(
  -3,
  -5));

    }

    @Test

    public void testMulMixedSign() {

        assertEquals(

            -15,

            hydra.lib.math.Mul.apply(
  3,
  -5));

    }

    @Test

    public void testMulWithZero() {

        assertEquals(

            0,

            hydra.lib.math.Mul.apply(
  42,
  0));

    }

    @Test

    public void testMulWithOne() {

        assertEquals(

            42,

            hydra.lib.math.Mul.apply(
  42,
  1));

    }

    // negate

    @Test

    public void testNegatePositive() {

        assertEquals(

            -5,

            hydra.lib.math.Negate.apply(5));

    }

    @Test

    public void testNegateNegative() {

        assertEquals(

            5,

            hydra.lib.math.Negate.apply(-5));

    }

    @Test

    public void testNegateZero() {

        assertEquals(

            0,

            hydra.lib.math.Negate.apply(0));

    }

    // odd

    @Test

    public void testOddOddPositive() {

        assertEquals(

            true,

            hydra.lib.math.Odd.apply(5));

    }

    @Test

    public void testOddEvenPositive() {

        assertEquals(

            false,

            hydra.lib.math.Odd.apply(4));

    }

    @Test

    public void testOddOddNegative() {

        assertEquals(

            true,

            hydra.lib.math.Odd.apply(-5));

    }

    @Test

    public void testOddEvenNegative() {

        assertEquals(

            false,

            hydra.lib.math.Odd.apply(-4));

    }

    @Test

    public void testOddZero() {

        assertEquals(

            false,

            hydra.lib.math.Odd.apply(0));

    }

    // pred

    @Test

    public void testPredPositive() {

        assertEquals(

            4,

            hydra.lib.math.Pred.apply(5));

    }

    @Test

    public void testPredZero() {

        assertEquals(

            -1,

            hydra.lib.math.Pred.apply(0));

    }

    @Test

    public void testPredNegative() {

        assertEquals(

            -6,

            hydra.lib.math.Pred.apply(-5));

    }

    // range

    @Test

    public void testRangeAscendingRange() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3,
  4,
  5),

            hydra.lib.math.Range.apply(
  1,
  5));

    }

    @Test

    public void testRangeSingleElement() {

        assertEquals(

            hydra.util.ConsList.of(5),

            hydra.lib.math.Range.apply(
  5,
  5));

    }

    @Test

    public void testRangeTwoElements() {

        assertEquals(

            hydra.util.ConsList.of(
  3,
  4),

            hydra.lib.math.Range.apply(
  3,
  4));

    }

    @Test

    public void testRangeNegativeStart() {

        assertEquals(

            hydra.util.ConsList.of(
  -2,
  -1,
  0,
  1,
  2),

            hydra.lib.math.Range.apply(
  -2,
  2));

    }

    // rem

    @Test

    public void testRemBasicRemainder() {

        assertEquals(

            1,

            hydra.lib.math.Rem.apply(
  10,
  3));

    }

    @Test

    public void testRemExactDivision() {

        assertEquals(

            0,

            hydra.lib.math.Rem.apply(
  10,
  2));

    }

    @Test

    public void testRemNegativeDividend() {

        assertEquals(

            -1,

            hydra.lib.math.Rem.apply(
  -10,
  3));

    }

    @Test

    public void testRemNegativeDivisor() {

        assertEquals(

            1,

            hydra.lib.math.Rem.apply(
  10,
  -3));

    }

    // signum

    @Test

    public void testSignumPositive() {

        assertEquals(

            1,

            hydra.lib.math.Signum.apply(5));

    }

    @Test

    public void testSignumNegative() {

        assertEquals(

            -1,

            hydra.lib.math.Signum.apply(-5));

    }

    @Test

    public void testSignumZero() {

        assertEquals(

            0,

            hydra.lib.math.Signum.apply(0));

    }

    // sub

    @Test

    public void testSubPositiveNumbers() {

        assertEquals(

            7,

            hydra.lib.math.Sub.apply(
  10,
  3));

    }

    @Test

    public void testSubNegativeNumbers() {

        assertEquals(

            -7,

            hydra.lib.math.Sub.apply(
  -10,
  -3));

    }

    @Test

    public void testSubMixedSign() {

        assertEquals(

            13,

            hydra.lib.math.Sub.apply(
  10,
  -3));

    }

    @Test

    public void testSubWithZero() {

        assertEquals(

            42,

            hydra.lib.math.Sub.apply(
  42,
  0));

    }

    // succ

    @Test

    public void testSuccPositive() {

        assertEquals(

            6,

            hydra.lib.math.Succ.apply(5));

    }

    @Test

    public void testSuccZero() {

        assertEquals(

            1,

            hydra.lib.math.Succ.apply(0));

    }

    @Test

    public void testSuccNegative() {

        assertEquals(

            -4,

            hydra.lib.math.Succ.apply(-5));

    }

    // e

    @Test

    public void testEEulerSNumber() {

        assertEquals(

            2.718281828459045,

            hydra.lib.math.E.apply(),

            1e-15);

    }

    // pi

    @Test

    public void testPiPiConstant() {

        assertEquals(

            3.141592653589793,

            hydra.lib.math.Pi.apply(),

            1e-15);

    }

    // sin

    @Test

    public void testSinSin0() {

        assertEquals(

            0.0,

            hydra.lib.math.Sin.apply(0.0),

            1e-15);

    }

    @Test

    public void testSinSinPiDiv2() {

        assertEquals(

            1.0,

            hydra.lib.math.Sin.apply(1.5707963267948966),

            1e-15);

    }

    @Test

    public void testSinSinPi() {

        assertEquals(

            1.2246467991473532e-16,

            hydra.lib.math.Sin.apply(3.141592653589793),

            1e-15);

    }

    @Test

    public void testSinSin1() {

        assertEquals(

            0.841470984808,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Sin.apply(1.0)),

            1e-15);

    }

    @Test

    public void testSinSin0dot5() {

        assertEquals(

            0.479425538604,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Sin.apply(0.5)),

            1e-15);

    }

    // cos

    @Test

    public void testCosCos0() {

        assertEquals(

            1.0,

            hydra.lib.math.Cos.apply(0.0),

            1e-15);

    }

    @Test

    public void testCosCosPiDiv2() {

        assertEquals(

            6.123233995736766e-17,

            hydra.lib.math.Cos.apply(1.5707963267948966),

            1e-15);

    }

    @Test

    public void testCosCosPi() {

        assertEquals(

            -1.0,

            hydra.lib.math.Cos.apply(3.141592653589793),

            1e-15);

    }

    @Test

    public void testCosCos1() {

        assertEquals(

            0.540302305868,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Cos.apply(1.0)),

            1e-15);

    }

    @Test

    public void testCosCos0dot5() {

        assertEquals(

            0.87758256189,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Cos.apply(0.5)),

            1e-15);

    }

    // tan

    @Test

    public void testTanTan0() {

        assertEquals(

            0.0,

            hydra.lib.math.Tan.apply(0.0),

            1e-15);

    }

    @Test

    public void testTanTanPiDiv4() {

        assertEquals(

            0.9999999999999999,

            hydra.lib.math.Tan.apply(0.7853981633974483),

            1e-15);

    }

    @Test

    public void testTanTan1() {

        assertEquals(

            1.55740772465,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Tan.apply(1.0)),

            1e-15);

    }

    @Test

    public void testTanTan0dot5() {

        assertEquals(

            0.546302489844,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Tan.apply(0.5)),

            1e-15);

    }

    // asin

    @Test

    public void testAsinAsin0() {

        assertEquals(

            0.0,

            hydra.lib.math.Asin.apply(0.0),

            1e-15);

    }

    @Test

    public void testAsinAsin1() {

        assertEquals(

            1.5707963267948966,

            hydra.lib.math.Asin.apply(1.0),

            1e-15);

    }

    @Test

    public void testAsinAsinNeg1() {

        assertEquals(

            -1.5707963267948966,

            hydra.lib.math.Asin.apply(-1.0),

            1e-15);

    }

    @Test

    public void testAsinAsin0dot5() {

        assertEquals(

            0.523598775598,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Asin.apply(0.5)),

            1e-15);

    }

    // acos

    @Test

    public void testAcosAcos1() {

        assertEquals(

            0.0,

            hydra.lib.math.Acos.apply(1.0),

            1e-15);

    }

    @Test

    public void testAcosAcos0() {

        assertEquals(

            1.5707963267948966,

            hydra.lib.math.Acos.apply(0.0),

            1e-15);

    }

    @Test

    public void testAcosAcosNeg1() {

        assertEquals(

            3.141592653589793,

            hydra.lib.math.Acos.apply(-1.0),

            1e-15);

    }

    @Test

    public void testAcosAcos0dot5() {

        assertEquals(

            1.0471975512,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Acos.apply(0.5)),

            1e-15);

    }

    // atan

    @Test

    public void testAtanAtan0() {

        assertEquals(

            0.0,

            hydra.lib.math.Atan.apply(0.0),

            1e-15);

    }

    @Test

    public void testAtanAtan1() {

        assertEquals(

            0.7853981633974483,

            hydra.lib.math.Atan.apply(1.0),

            1e-15);

    }

    @Test

    public void testAtanAtan0dot5() {

        assertEquals(

            0.463647609001,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Atan.apply(0.5)),

            1e-15);

    }

    // atan2

    @Test

    public void testAtan2Atan211() {

        assertEquals(

            0.7853981633974483,

            hydra.lib.math.Atan2.apply(
  1.0,
  1.0),

            1e-15);

    }

    @Test

    public void testAtan2Atan210() {

        assertEquals(

            1.5707963267948966,

            hydra.lib.math.Atan2.apply(
  1.0,
  0.0),

            1e-15);

    }

    @Test

    public void testAtan2Atan201() {

        assertEquals(

            0.0,

            hydra.lib.math.Atan2.apply(
  0.0,
  1.0),

            1e-15);

    }

    @Test

    public void testAtan2Atan234() {

        assertEquals(

            0.643501108793,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Atan2.apply(
    3.0,
    4.0)),

            1e-15);

    }

    // sinh

    @Test

    public void testSinhSinh0() {

        assertEquals(

            0.0,

            hydra.lib.math.Sinh.apply(0.0),

            1e-15);

    }

    @Test

    public void testSinhSinh1() {

        assertEquals(

            1.1752011936438014,

            hydra.lib.math.Sinh.apply(1.0),

            1e-15);

    }

    @Test

    public void testSinhSinh2() {

        assertEquals(

            3.62686040785,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Sinh.apply(2.0)),

            1e-15);

    }

    // cosh

    @Test

    public void testCoshCosh0() {

        assertEquals(

            1.0,

            hydra.lib.math.Cosh.apply(0.0),

            1e-15);

    }

    @Test

    public void testCoshCosh1() {

        assertEquals(

            1.54308063482,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Cosh.apply(1.0)),

            1e-15);

    }

    @Test

    public void testCoshCosh2() {

        assertEquals(

            3.76219569108,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Cosh.apply(2.0)),

            1e-15);

    }

    // tanh

    @Test

    public void testTanhTanh0() {

        assertEquals(

            0.0,

            hydra.lib.math.Tanh.apply(0.0),

            1e-15);

    }

    @Test

    public void testTanhTanh1() {

        assertEquals(

            0.7615941559557649,

            hydra.lib.math.Tanh.apply(1.0),

            1e-15);

    }

    @Test

    public void testTanhTanh0dot5() {

        assertEquals(

            0.46211715726,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Tanh.apply(0.5)),

            1e-15);

    }

    // asinh

    @Test

    public void testAsinhAsinh0() {

        assertEquals(

            0.0,

            hydra.lib.math.Asinh.apply(0.0),

            1e-15);

    }

    @Test

    public void testAsinhAsinh1() {

        assertEquals(

            0.88137358702,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Asinh.apply(1.0)),

            1e-15);

    }

    @Test

    public void testAsinhAsinh0dot5() {

        assertEquals(

            0.48121182506,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Asinh.apply(0.5)),

            1e-15);

    }

    // acosh

    @Test

    public void testAcoshAcosh1() {

        assertEquals(

            0.0,

            hydra.lib.math.Acosh.apply(1.0),

            1e-15);

    }

    @Test

    public void testAcoshAcosh2() {

        assertEquals(

            1.31695789692,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Acosh.apply(2.0)),

            1e-15);

    }

    @Test

    public void testAcoshAcosh3() {

        assertEquals(

            1.76274717404,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Acosh.apply(3.0)),

            1e-15);

    }

    // atanh

    @Test

    public void testAtanhAtanh0() {

        assertEquals(

            0.0,

            hydra.lib.math.Atanh.apply(0.0),

            1e-15);

    }

    @Test

    public void testAtanhAtanh0dot5() {

        assertEquals(

            0.549306144334,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Atanh.apply(0.5)),

            1e-15);

    }

    @Test

    public void testAtanhAtanh0dot1() {

        assertEquals(

            0.100335347731,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Atanh.apply(0.1)),

            1e-15);

    }

    // exp

    @Test

    public void testExpExp0() {

        assertEquals(

            1.0,

            hydra.lib.math.Exp.apply(0.0),

            1e-15);

    }

    @Test

    public void testExpExp1() {

        assertEquals(

            2.71828182846,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Exp.apply(1.0)),

            1e-15);

    }

    @Test

    public void testExpExpNeg1() {

        assertEquals(

            0.367879441171,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Exp.apply(-1.0)),

            1e-15);

    }

    @Test

    public void testExpExp2() {

        assertEquals(

            7.38905609893,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Exp.apply(2.0)),

            1e-15);

    }

    @Test

    public void testExpExp0dot5() {

        assertEquals(

            1.6487212707,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Exp.apply(0.5)),

            1e-15);

    }

    // log

    @Test

    public void testLogLog1() {

        assertEquals(

            0.0,

            hydra.lib.math.Log.apply(1.0),

            1e-15);

    }

    @Test

    public void testLogLogE() {

        assertEquals(

            1.0,

            hydra.lib.math.Log.apply(2.718281828459045),

            1e-15);

    }

    @Test

    public void testLogLog2() {

        assertEquals(

            0.69314718056,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Log.apply(2.0)),

            1e-15);

    }

    @Test

    public void testLogLog10() {

        assertEquals(

            2.30258509299,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Log.apply(10.0)),

            1e-15);

    }

    // logBase

    @Test

    public void testLogbaseLog101() {

        assertEquals(

            0.0,

            hydra.lib.math.LogBase.apply(
  10.0,
  1.0),

            1e-15);

    }

    @Test

    public void testLogbaseLog1010() {

        assertEquals(

            1.0,

            hydra.lib.math.LogBase.apply(
  10.0,
  10.0),

            1e-15);

    }

    @Test

    public void testLogbaseLog10100() {

        assertEquals(

            2.0,

            hydra.lib.math.LogBase.apply(
  10.0,
  100.0),

            1e-15);

    }

    @Test

    public void testLogbaseLog28() {

        assertEquals(

            3.0,

            hydra.lib.math.LogBase.apply(
  2.0,
  8.0),

            1e-15);

    }

    @Test

    public void testLogbaseLog210() {

        assertEquals(

            3.32192809489,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.LogBase.apply(
    2.0,
    10.0)),

            1e-15);

    }

    // pow

    @Test

    public void testPow23() {

        assertEquals(

            8.0,

            hydra.lib.math.Pow.apply(
  2.0,
  3.0),

            1e-15);

    }

    @Test

    public void testPow100() {

        assertEquals(

            1.0,

            hydra.lib.math.Pow.apply(
  10.0,
  0.0),

            1e-15);

    }

    @Test

    public void testPow2Neg1() {

        assertEquals(

            0.5,

            hydra.lib.math.Pow.apply(
  2.0,
  -1.0),

            1e-15);

    }

    @Test

    public void testPow20dot5() {

        assertEquals(

            1.41421356237,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Pow.apply(
    2.0,
    0.5)),

            1e-15);

    }

    // sqrt

    @Test

    public void testSqrtSqrt4() {

        assertEquals(

            2.0,

            hydra.lib.math.Sqrt.apply(4.0),

            1e-15);

    }

    @Test

    public void testSqrtSqrt9() {

        assertEquals(

            3.0,

            hydra.lib.math.Sqrt.apply(9.0),

            1e-15);

    }

    @Test

    public void testSqrtSqrt2() {

        assertEquals(

            1.4142135623730951,

            hydra.lib.math.Sqrt.apply(2.0),

            1e-15);

    }

    @Test

    public void testSqrtSqrt0() {

        assertEquals(

            0.0,

            hydra.lib.math.Sqrt.apply(0.0),

            1e-15);

    }

    @Test

    public void testSqrtSqrt3() {

        assertEquals(

            1.73205080757,

            hydra.lib.math.RoundFloat64.apply(
  12,
  hydra.lib.math.Sqrt.apply(3.0)),

            1e-15);

    }

    // ceiling

    @Test

    public void testCeilingCeiling3dot2() {

        assertEquals(

            new java.math.BigInteger("4"),

            hydra.lib.math.Ceiling.apply(3.2));

    }

    @Test

    public void testCeilingCeiling3dot0() {

        assertEquals(

            new java.math.BigInteger("3"),

            hydra.lib.math.Ceiling.apply(3.0));

    }

    @Test

    public void testCeilingCeilingNeg3dot2() {

        assertEquals(

            new java.math.BigInteger("-3"),

            hydra.lib.math.Ceiling.apply(-3.2));

    }

    @Test

    public void testCeilingCeilingNeg3dot0() {

        assertEquals(

            new java.math.BigInteger("-3"),

            hydra.lib.math.Ceiling.apply(-3.0));

    }

    // floor

    @Test

    public void testFloorFloor3dot8() {

        assertEquals(

            new java.math.BigInteger("3"),

            hydra.lib.math.Floor.apply(3.8));

    }

    @Test

    public void testFloorFloor3dot0() {

        assertEquals(

            new java.math.BigInteger("3"),

            hydra.lib.math.Floor.apply(3.0));

    }

    @Test

    public void testFloorFloorNeg3dot2() {

        assertEquals(

            new java.math.BigInteger("-4"),

            hydra.lib.math.Floor.apply(-3.2));

    }

    @Test

    public void testFloorFloorNeg3dot0() {

        assertEquals(

            new java.math.BigInteger("-3"),

            hydra.lib.math.Floor.apply(-3.0));

    }

    // round

    @Test

    public void testRoundRound3dot4() {

        assertEquals(

            new java.math.BigInteger("3"),

            hydra.lib.math.Round.apply(3.4));

    }

    @Test

    public void testRoundRound3dot5() {

        assertEquals(

            new java.math.BigInteger("4"),

            hydra.lib.math.Round.apply(3.5));

    }

    @Test

    public void testRoundRound3dot6() {

        assertEquals(

            new java.math.BigInteger("4"),

            hydra.lib.math.Round.apply(3.6));

    }

    @Test

    public void testRoundRoundNeg3dot4() {

        assertEquals(

            new java.math.BigInteger("-3"),

            hydra.lib.math.Round.apply(-3.4));

    }

    @Test

    public void testRoundRoundNeg3dot5() {

        assertEquals(

            new java.math.BigInteger("-4"),

            hydra.lib.math.Round.apply(-3.5));

    }

    // roundBigfloat

    @Test

    public void testRoundbigfloatZero() {

        assertEquals(0, (new java.math.BigDecimal("0.0")).compareTo(hydra.lib.math.RoundBigfloat.apply(
  5,
  new java.math.BigDecimal("0.0"))));

    }

    @Test

    public void testRoundbigfloatRoundPiTo4Digits() {

        assertEquals(0, (new java.math.BigDecimal("3.142")).compareTo(hydra.lib.math.RoundBigfloat.apply(
  4,
  new java.math.BigDecimal("3.141592653589793"))));

    }

    @Test

    public void testRoundbigfloatRound1234dot5To3Digits() {

        assertEquals(0, (new java.math.BigDecimal("1230.0")).compareTo(hydra.lib.math.RoundBigfloat.apply(
  3,
  new java.math.BigDecimal("1234.5"))));

    }

    @Test

    public void testRoundbigfloatRound0dot001234To2Digits() {

        assertEquals(0, (new java.math.BigDecimal("1.2e-3")).compareTo(hydra.lib.math.RoundBigfloat.apply(
  2,
  new java.math.BigDecimal("1.234e-3"))));

    }

    @Test

    public void testRoundbigfloatNegative() {

        assertEquals(0, (new java.math.BigDecimal("-1230.0")).compareTo(hydra.lib.math.RoundBigfloat.apply(
  3,
  new java.math.BigDecimal("-1234.5"))));

    }

    // roundFloat32

    @Test

    public void testRoundfloat32Zero() {

        assertEquals(

            (float) (0.0),

            hydra.lib.math.RoundFloat32.apply(
  5,
  (float) (0.0)),

            1e-15);

    }

    @Test

    public void testRoundfloat32RoundPiTo4Digits() {

        assertEquals(

            (float) (3.1419999599456787),

            hydra.lib.math.RoundFloat32.apply(
  4,
  (float) (3.1415927410125732)),

            1e-15);

    }

    @Test

    public void testRoundfloat32Round1234dot5To3Digits() {

        assertEquals(

            (float) (1230.0),

            hydra.lib.math.RoundFloat32.apply(
  3,
  (float) (1234.5)),

            1e-15);

    }

    @Test

    public void testRoundfloat32Negative() {

        assertEquals(

            (float) (-1230.0),

            hydra.lib.math.RoundFloat32.apply(
  3,
  (float) (-1234.5)),

            1e-15);

    }

    // roundFloat64

    @Test

    public void testRoundfloat64Zero() {

        assertEquals(

            0.0,

            hydra.lib.math.RoundFloat64.apply(
  5,
  0.0),

            1e-15);

    }

    @Test

    public void testRoundfloat64RoundPiTo4Digits() {

        assertEquals(

            3.142,

            hydra.lib.math.RoundFloat64.apply(
  4,
  3.141592653589793),

            1e-15);

    }

    @Test

    public void testRoundfloat64RoundPiTo10Digits() {

        assertEquals(

            3.141592654,

            hydra.lib.math.RoundFloat64.apply(
  10,
  3.141592653589793),

            1e-15);

    }

    @Test

    public void testRoundfloat64Round1234dot5To3Digits() {

        assertEquals(

            1230.0,

            hydra.lib.math.RoundFloat64.apply(
  3,
  1234.5),

            1e-15);

    }

    @Test

    public void testRoundfloat64Round0dot001234To2Digits() {

        assertEquals(

            1.2e-3,

            hydra.lib.math.RoundFloat64.apply(
  2,
  1.234e-3),

            1e-15);

    }

    @Test

    public void testRoundfloat64Negative() {

        assertEquals(

            -1230.0,

            hydra.lib.math.RoundFloat64.apply(
  3,
  -1234.5),

            1e-15);

    }

    @Test

    public void testRoundfloat64Round1Digit() {

        assertEquals(

            10.0,

            hydra.lib.math.RoundFloat64.apply(
  1,
  9.876),

            1e-15);

    }

    // truncate

    @Test

    public void testTruncateTruncate3dot8() {

        assertEquals(

            new java.math.BigInteger("3"),

            hydra.lib.math.Truncate.apply(3.8));

    }

    @Test

    public void testTruncateTruncate3dot2() {

        assertEquals(

            new java.math.BigInteger("3"),

            hydra.lib.math.Truncate.apply(3.2));

    }

    @Test

    public void testTruncateTruncateNeg3dot8() {

        assertEquals(

            new java.math.BigInteger("-3"),

            hydra.lib.math.Truncate.apply(-3.8));

    }

    @Test

    public void testTruncateTruncateNeg3dot2() {

        assertEquals(

            new java.math.BigInteger("-3"),

            hydra.lib.math.Truncate.apply(-3.2));

    }
}
