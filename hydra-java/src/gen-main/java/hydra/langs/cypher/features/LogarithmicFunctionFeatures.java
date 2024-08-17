// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Logarithmic functions
 */
public class LogarithmicFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.LogarithmicFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_E = new hydra.core.Name("e");
  
  public static final hydra.core.Name FIELD_NAME_EXP = new hydra.core.Name("exp");
  
  public static final hydra.core.Name FIELD_NAME_LOG = new hydra.core.Name("log");
  
  public static final hydra.core.Name FIELD_NAME_LOG10 = new hydra.core.Name("log10");
  
  public static final hydra.core.Name FIELD_NAME_SQRT = new hydra.core.Name("sqrt");
  
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
    java.util.Objects.requireNonNull((e));
    java.util.Objects.requireNonNull((exp));
    java.util.Objects.requireNonNull((log));
    java.util.Objects.requireNonNull((log10));
    java.util.Objects.requireNonNull((sqrt));
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
    LogarithmicFunctionFeatures o = (LogarithmicFunctionFeatures) (other);
    return e.equals(o.e) && exp.equals(o.exp) && log.equals(o.log) && log10.equals(o.log10) && sqrt.equals(o.sqrt);
  }
  
  @Override
  public int hashCode() {
    return 2 * e.hashCode() + 3 * exp.hashCode() + 5 * log.hashCode() + 7 * log10.hashCode() + 11 * sqrt.hashCode();
  }
  
  public LogarithmicFunctionFeatures withE(Boolean e) {
    java.util.Objects.requireNonNull((e));
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }
  
  public LogarithmicFunctionFeatures withExp(Boolean exp) {
    java.util.Objects.requireNonNull((exp));
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }
  
  public LogarithmicFunctionFeatures withLog(Boolean log) {
    java.util.Objects.requireNonNull((log));
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }
  
  public LogarithmicFunctionFeatures withLog10(Boolean log10) {
    java.util.Objects.requireNonNull((log10));
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }
  
  public LogarithmicFunctionFeatures withSqrt(Boolean sqrt) {
    java.util.Objects.requireNonNull((sqrt));
    return new LogarithmicFunctionFeatures(e, exp, log, log10, sqrt);
  }
}