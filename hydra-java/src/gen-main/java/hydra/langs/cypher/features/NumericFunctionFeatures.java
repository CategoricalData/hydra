// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Numeric functions
 */
public class NumericFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.NumericFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ABS = new hydra.core.Name("abs");
  
  public static final hydra.core.Name FIELD_NAME_CEIL = new hydra.core.Name("ceil");
  
  public static final hydra.core.Name FIELD_NAME_FLOOR = new hydra.core.Name("floor");
  
  public static final hydra.core.Name FIELD_NAME_IS_NA_N = new hydra.core.Name("isNaN");
  
  public static final hydra.core.Name FIELD_NAME_RAND = new hydra.core.Name("rand");
  
  public static final hydra.core.Name FIELD_NAME_ROUND = new hydra.core.Name("round");
  
  public static final hydra.core.Name FIELD_NAME_SIGN = new hydra.core.Name("sign");
  
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
    java.util.Objects.requireNonNull((abs));
    java.util.Objects.requireNonNull((ceil));
    java.util.Objects.requireNonNull((floor));
    java.util.Objects.requireNonNull((isNaN));
    java.util.Objects.requireNonNull((rand));
    java.util.Objects.requireNonNull((round));
    java.util.Objects.requireNonNull((sign));
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
    NumericFunctionFeatures o = (NumericFunctionFeatures) (other);
    return abs.equals(o.abs) && ceil.equals(o.ceil) && floor.equals(o.floor) && isNaN.equals(o.isNaN) && rand.equals(o.rand) && round.equals(o.round) && sign.equals(o.sign);
  }
  
  @Override
  public int hashCode() {
    return 2 * abs.hashCode() + 3 * ceil.hashCode() + 5 * floor.hashCode() + 7 * isNaN.hashCode() + 11 * rand.hashCode() + 13 * round.hashCode() + 17 * sign.hashCode();
  }
  
  public NumericFunctionFeatures withAbs(Boolean abs) {
    java.util.Objects.requireNonNull((abs));
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }
  
  public NumericFunctionFeatures withCeil(Boolean ceil) {
    java.util.Objects.requireNonNull((ceil));
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }
  
  public NumericFunctionFeatures withFloor(Boolean floor) {
    java.util.Objects.requireNonNull((floor));
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }
  
  public NumericFunctionFeatures withIsNaN(Boolean isNaN) {
    java.util.Objects.requireNonNull((isNaN));
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }
  
  public NumericFunctionFeatures withRand(Boolean rand) {
    java.util.Objects.requireNonNull((rand));
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }
  
  public NumericFunctionFeatures withRound(Boolean round) {
    java.util.Objects.requireNonNull((round));
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }
  
  public NumericFunctionFeatures withSign(Boolean sign) {
    java.util.Objects.requireNonNull((sign));
    return new NumericFunctionFeatures(abs, ceil, floor, isNaN, rand, round, sign);
  }
}