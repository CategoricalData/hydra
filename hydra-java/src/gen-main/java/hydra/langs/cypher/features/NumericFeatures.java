// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for numeric functions.
 */
public class NumericFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.NumericFeatures");
  
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
    if (abs == null) {
      throw new IllegalArgumentException("null value for 'abs' argument");
    }
    if (ceil == null) {
      throw new IllegalArgumentException("null value for 'ceil' argument");
    }
    if (e == null) {
      throw new IllegalArgumentException("null value for 'e' argument");
    }
    if (exp == null) {
      throw new IllegalArgumentException("null value for 'exp' argument");
    }
    if (floor == null) {
      throw new IllegalArgumentException("null value for 'floor' argument");
    }
    if (isNaN == null) {
      throw new IllegalArgumentException("null value for 'isNaN' argument");
    }
    if (log == null) {
      throw new IllegalArgumentException("null value for 'log' argument");
    }
    if (log10 == null) {
      throw new IllegalArgumentException("null value for 'log10' argument");
    }
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
    if (round == null) {
      throw new IllegalArgumentException("null value for 'round' argument");
    }
    if (sign == null) {
      throw new IllegalArgumentException("null value for 'sign' argument");
    }
    if (sqrt == null) {
      throw new IllegalArgumentException("null value for 'sqrt' argument");
    }
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
    if (abs == null) {
      throw new IllegalArgumentException("null value for 'abs' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withCeil(Boolean ceil) {
    if (ceil == null) {
      throw new IllegalArgumentException("null value for 'ceil' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withE(Boolean e) {
    if (e == null) {
      throw new IllegalArgumentException("null value for 'e' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withExp(Boolean exp) {
    if (exp == null) {
      throw new IllegalArgumentException("null value for 'exp' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withFloor(Boolean floor) {
    if (floor == null) {
      throw new IllegalArgumentException("null value for 'floor' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withIsNaN(Boolean isNaN) {
    if (isNaN == null) {
      throw new IllegalArgumentException("null value for 'isNaN' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withLog(Boolean log) {
    if (log == null) {
      throw new IllegalArgumentException("null value for 'log' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withLog10(Boolean log10) {
    if (log10 == null) {
      throw new IllegalArgumentException("null value for 'log10' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withRange(Boolean range) {
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withRound(Boolean round) {
    if (round == null) {
      throw new IllegalArgumentException("null value for 'round' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withSign(Boolean sign) {
    if (sign == null) {
      throw new IllegalArgumentException("null value for 'sign' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
  
  public NumericFeatures withSqrt(Boolean sqrt) {
    if (sqrt == null) {
      throw new IllegalArgumentException("null value for 'sqrt' argument");
    }
    return new NumericFeatures(abs, ceil, e, exp, floor, isNaN, log, log10, range, round, sign, sqrt);
  }
}