// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for numeric functions.
 */
public class NumericFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.NumericFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ABS = new hydra.core.Name("abs");
  
  public static final hydra.core.Name FIELD_NAME_CEIL = new hydra.core.Name("ceil");
  
  public static final hydra.core.Name FIELD_NAME_E = new hydra.core.Name("e");
  
  public static final hydra.core.Name FIELD_NAME_EXP = new hydra.core.Name("exp");
  
  public static final hydra.core.Name FIELD_NAME_FLOOR = new hydra.core.Name("floor");
  
  public static final hydra.core.Name FIELD_NAME_IS_NA_N = new hydra.core.Name("isNaN");
  
  public static final hydra.core.Name FIELD_NAME_LOG = new hydra.core.Name("log");
  
  public static final hydra.core.Name FIELD_NAME_LOG10 = new hydra.core.Name("log10");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public static final hydra.core.Name FIELD_NAME_ROUND = new hydra.core.Name("round");
  
  public static final hydra.core.Name FIELD_NAME_SIGN = new hydra.core.Name("sign");
  
  public static final hydra.core.Name FIELD_NAME_SQRT = new hydra.core.Name("sqrt");
  
  /**
   * Whether to expect the abs() function.
   */
  public final Boolean abs;
  
  /**
   * Whether to expect the ceil() function.
   */
  public final Boolean ceil;
  
  /**
   * Whether to expect the e() function.
   */
  public final Boolean e;
  
  /**
   * Whether to expect the exp() function.
   */
  public final Boolean exp;
  
  /**
   * Whether to expect the floor() function.
   */
  public final Boolean floor;
  
  /**
   * Whether to expect the isNaN() function.
   */
  public final Boolean isNaN;
  
  /**
   * Whether to expect the log() function.
   */
  public final Boolean log;
  
  /**
   * Whether to expect the log10() function.
   */
  public final Boolean log10;
  
  /**
   * Whether to expect the range() function.
   */
  public final Boolean range;
  
  /**
   * Whether to expect the round() function.
   */
  public final Boolean round;
  
  /**
   * Whether to expect the sign() function.
   */
  public final Boolean sign;
  
  /**
   * Whether to expect the sqrt() function.
   */
  public final Boolean sqrt;
  
  public NumericFeatures (Boolean abs, Boolean ceil, Boolean e, Boolean exp, Boolean floor, Boolean isNaN, Boolean log, Boolean log10, Boolean range, Boolean round, Boolean sign, Boolean sqrt) {
    java.util.Objects.requireNonNull((abs));
    java.util.Objects.requireNonNull((ceil));
    java.util.Objects.requireNonNull((e));
    java.util.Objects.requireNonNull((exp));
    java.util.Objects.requireNonNull((floor));
    java.util.Objects.requireNonNull((isNaN));
    java.util.Objects.requireNonNull((log));
    java.util.Objects.requireNonNull((log10));
    java.util.Objects.requireNonNull((range));
    java.util.Objects.requireNonNull((round));
    java.util.Objects.requireNonNull((sign));
    java.util.Objects.requireNonNull((sqrt));
    this.abs = abs;
    this.ceil = ceil;
    this.e = e;
    this.exp = exp;
    this.floor = floor;
    this.isNaN = isNaN;
    this.log = log;
    this.log10 = log10;
    this.range = range;
    this.round = round;
    this.sign = sign;
    this.sqrt = sqrt;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NumericFeatures)) {
      return false;
    }
    NumericFeatures o = (NumericFeatures) (other);
    return abs.equals(o.abs) && ceil.equals(o.ceil) && e.equals(o.e) && exp.equals(o.exp) && floor.equals(o.floor) && isNaN.equals(o.isNaN) && log.equals(o.log) && log10.equals(o.log10) && range.equals(o.range) && round.equals(o.round) && sign.equals(o.sign) && sqrt.equals(o.sqrt);
  }
  
  @Override
  public int hashCode() {
    return 2 * abs.hashCode() + 3 * ceil.hashCode() + 5 * e.hashCode() + 7 * exp.hashCode() + 11 * floor.hashCode() + 13 * isNaN.hashCode() + 17 * log.hashCode() + 19 * log10.hashCode() + 23 * range.hashCode() + 29 * round.hashCode() + 31 * sign.hashCode() + 37 * sqrt.hashCode();
  }
  
  public NumericFeatures withAbs(Boolean abs) {
    java.util.Objects.requireNonNull((abs));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withCeil(Boolean ceil) {
    java.util.Objects.requireNonNull((ceil));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withE(Boolean e) {
    java.util.Objects.requireNonNull((e));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withExp(Boolean exp) {
    java.util.Objects.requireNonNull((exp));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withFloor(Boolean floor) {
    java.util.Objects.requireNonNull((floor));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withIsNaN(Boolean isNaN) {
    java.util.Objects.requireNonNull((isNaN));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withLog(Boolean log) {
    java.util.Objects.requireNonNull((log));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withLog10(Boolean log10) {
    java.util.Objects.requireNonNull((log10));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withRange(Boolean range) {
    java.util.Objects.requireNonNull((range));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withRound(Boolean round) {
    java.util.Objects.requireNonNull((round));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withSign(Boolean sign) {
    java.util.Objects.requireNonNull((sign));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withSqrt(Boolean sqrt) {
    java.util.Objects.requireNonNull((sqrt));
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
}