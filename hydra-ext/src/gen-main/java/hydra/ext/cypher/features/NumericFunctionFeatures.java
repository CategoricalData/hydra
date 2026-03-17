// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Numeric functions
 */
public class NumericFunctionFeatures implements Serializable, Comparable<NumericFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.NumericFunctionFeatures");

  public static final hydra.core.Name ABS = new hydra.core.Name("abs");

  public static final hydra.core.Name CEIL = new hydra.core.Name("ceil");

  public static final hydra.core.Name FLOOR = new hydra.core.Name("floor");

  public static final hydra.core.Name IS_NA_N = new hydra.core.Name("isNaN");

  public static final hydra.core.Name RAND = new hydra.core.Name("rand");

  public static final hydra.core.Name ROUND = new hydra.core.Name("round");

  public static final hydra.core.Name SIGN = new hydra.core.Name("sign");

  /**
   * The abs() function. Returns the absolute value of a FLOAT.; Returns the absolute value of an INTEGER.
   */
  public final Boolean abs;

  /**
   * The ceil() function. Returns the smallest FLOAT that is greater than or equal to a number and equal to an INTEGER.
   */
  public final Boolean ceil;

  /**
   * The floor() function. Returns the largest FLOAT that is less than or equal to a number and equal to an INTEGER.
   */
  public final Boolean floor;

  /**
   * The isNaN() function. Returns true if the floating point number is NaN.; Returns true if the integer number is NaN.
   */
  public final Boolean isNaN;

  /**
   * The rand() function. Returns a random FLOAT in the range from 0 (inclusive) to 1 (exclusive).
   */
  public final Boolean rand;

  /**
   * The round() function. Returns the value of a number rounded to the nearest INTEGER.; Returns the value of a number rounded to the specified precision using rounding mode HALF_UP.; Returns the value of a number rounded to the specified precision with the specified rounding mode.
   */
  public final Boolean round;

  /**
   * The sign() function. Returns the signum of a FLOAT: 0 if the number is 0, -1 for any negative number, and 1 for any positive number.; Returns the signum of an INTEGER: 0 if the number is 0, -1 for any negative number, and 1 for any positive number.
   */
  public final Boolean sign;

  public NumericFunctionFeatures (Boolean abs, Boolean ceil, Boolean floor, Boolean isNaN, Boolean rand, Boolean round, Boolean sign) {
    this.abs = abs;
    this.ceil = ceil;
    this.floor = floor;
    this.isNaN = isNaN;
    this.rand = rand;
    this.round = round;
    this.sign = sign;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NumericFunctionFeatures)) {
      return false;
    }
    NumericFunctionFeatures o = (NumericFunctionFeatures) other;
    return java.util.Objects.equals(
      this.abs,
      o.abs) && java.util.Objects.equals(
      this.ceil,
      o.ceil) && java.util.Objects.equals(
      this.floor,
      o.floor) && java.util.Objects.equals(
      this.isNaN,
      o.isNaN) && java.util.Objects.equals(
      this.rand,
      o.rand) && java.util.Objects.equals(
      this.round,
      o.round) && java.util.Objects.equals(
      this.sign,
      o.sign);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(abs) + 3 * java.util.Objects.hashCode(ceil) + 5 * java.util.Objects.hashCode(floor) + 7 * java.util.Objects.hashCode(isNaN) + 11 * java.util.Objects.hashCode(rand) + 13 * java.util.Objects.hashCode(round) + 17 * java.util.Objects.hashCode(sign);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NumericFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) abs).compareTo(other.abs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) ceil).compareTo(other.ceil);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) floor).compareTo(other.floor);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) isNaN).compareTo(other.isNaN);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) rand).compareTo(other.rand);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) round).compareTo(other.round);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) sign).compareTo(other.sign);
  }

  public NumericFunctionFeatures withAbs(Boolean abs) {
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }

  public NumericFunctionFeatures withCeil(Boolean ceil) {
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }

  public NumericFunctionFeatures withFloor(Boolean floor) {
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }

  public NumericFunctionFeatures withIsNaN(Boolean isNaN) {
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }

  public NumericFunctionFeatures withRand(Boolean rand) {
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }

  public NumericFunctionFeatures withRound(Boolean round) {
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }

  public NumericFunctionFeatures withSign(Boolean sign) {
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }
}
