// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Logarithmic functions
 */
public class LogarithmicFunctionFeatures implements Serializable, Comparable<LogarithmicFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.LogarithmicFunctionFeatures");

  public static final hydra.core.Name E = new hydra.core.Name("e");

  public static final hydra.core.Name EXP = new hydra.core.Name("exp");

  public static final hydra.core.Name LOG = new hydra.core.Name("log");

  public static final hydra.core.Name LOG10 = new hydra.core.Name("log10");

  public static final hydra.core.Name SQRT = new hydra.core.Name("sqrt");

  /**
   * The e() function. Returns the base of the natural logarithm, e.
   */
  public final Boolean e;

  /**
   * The exp() function. Returns e^n, where e is the base of the natural logarithm, and n is the value of the argument expression.
   */
  public final Boolean exp;

  /**
   * The log() function. Returns the natural logarithm of a FLOAT.
   */
  public final Boolean log;

  /**
   * The log10() function. Returns the common logarithm (base 10) of a FLOAT.
   */
  public final Boolean log10;

  /**
   * The sqrt() function. Returns the square root of a FLOAT.
   */
  public final Boolean sqrt;

  public LogarithmicFunctionFeatures (Boolean e, Boolean exp, Boolean log, Boolean log10, Boolean sqrt) {
    this.e = e;
    this.exp = exp;
    this.log = log;
    this.log10 = log10;
    this.sqrt = sqrt;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LogarithmicFunctionFeatures)) {
      return false;
    }
    LogarithmicFunctionFeatures o = (LogarithmicFunctionFeatures) other;
    return java.util.Objects.equals(
      this.e,
      o.e) && java.util.Objects.equals(
      this.exp,
      o.exp) && java.util.Objects.equals(
      this.log,
      o.log) && java.util.Objects.equals(
      this.log10,
      o.log10) && java.util.Objects.equals(
      this.sqrt,
      o.sqrt);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(e) + 3 * java.util.Objects.hashCode(exp) + 5 * java.util.Objects.hashCode(log) + 7 * java.util.Objects.hashCode(log10) + 11 * java.util.Objects.hashCode(sqrt);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LogarithmicFunctionFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      e,
      other.e);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      exp,
      other.exp);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      log,
      other.log);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      log10,
      other.log10);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sqrt,
      other.sqrt);
  }

  public LogarithmicFunctionFeatures withE(Boolean e) {
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }

  public LogarithmicFunctionFeatures withExp(Boolean exp) {
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }

  public LogarithmicFunctionFeatures withLog(Boolean log) {
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }

  public LogarithmicFunctionFeatures withLog10(Boolean log10) {
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }

  public LogarithmicFunctionFeatures withSqrt(Boolean sqrt) {
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }
}
