// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Trigonometric functions
 */
public class TrigonometricFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.features.TrigonometricFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ACOS = new hydra.core.Name("acos");
  
  public static final hydra.core.Name FIELD_NAME_ASIN = new hydra.core.Name("asin");
  
  public static final hydra.core.Name FIELD_NAME_ATAN = new hydra.core.Name("atan");
  
  public static final hydra.core.Name FIELD_NAME_ATAN2 = new hydra.core.Name("atan2");
  
  public static final hydra.core.Name FIELD_NAME_COS = new hydra.core.Name("cos");
  
  public static final hydra.core.Name FIELD_NAME_COT = new hydra.core.Name("cot");
  
  public static final hydra.core.Name FIELD_NAME_DEGREES = new hydra.core.Name("degrees");
  
  public static final hydra.core.Name FIELD_NAME_HAVERSIN = new hydra.core.Name("haversin");
  
  public static final hydra.core.Name FIELD_NAME_PI = new hydra.core.Name("pi");
  
  public static final hydra.core.Name FIELD_NAME_RADIANS = new hydra.core.Name("radians");
  
  public static final hydra.core.Name FIELD_NAME_SIN = new hydra.core.Name("sin");
  
  public static final hydra.core.Name FIELD_NAME_TAN = new hydra.core.Name("tan");
  
  /**
   * The acos() function. Returns the arccosine of a FLOAT in radians.
   */
  public final Boolean acos;
  
  /**
   * The asin() function. Returns the arcsine of a FLOAT in radians.
   */
  public final Boolean asin;
  
  /**
   * The atan() function. Returns the arctangent of a FLOAT in radians.
   */
  public final Boolean atan;
  
  /**
   * The atan2() function. Returns the arctangent2 of a set of coordinates in radians.
   */
  public final Boolean atan2;
  
  /**
   * The cos() function. Returns the cosine of a FLOAT.
   */
  public final Boolean cos;
  
  /**
   * The cot() function. Returns the cotangent of a FLOAT.
   */
  public final Boolean cot;
  
  /**
   * The degrees() function. Converts radians to degrees.
   */
  public final Boolean degrees;
  
  /**
   * The haversin() function. Returns half the versine of a number.
   */
  public final Boolean haversin;
  
  /**
   * The pi() function. Returns the mathematical constant pi.
   */
  public final Boolean pi;
  
  /**
   * The radians() function. Converts degrees to radians.
   */
  public final Boolean radians;
  
  /**
   * The sin() function. Returns the sine of a FLOAT.
   */
  public final Boolean sin;
  
  /**
   * The tan() function. Returns the tangent of a FLOAT.
   */
  public final Boolean tan;
  
  public TrigonometricFunctionFeatures (Boolean acos, Boolean asin, Boolean atan, Boolean atan2, Boolean cos, Boolean cot, Boolean degrees, Boolean haversin, Boolean pi, Boolean radians, Boolean sin, Boolean tan) {
    java.util.Objects.requireNonNull((acos));
    java.util.Objects.requireNonNull((asin));
    java.util.Objects.requireNonNull((atan));
    java.util.Objects.requireNonNull((atan2));
    java.util.Objects.requireNonNull((cos));
    java.util.Objects.requireNonNull((cot));
    java.util.Objects.requireNonNull((degrees));
    java.util.Objects.requireNonNull((haversin));
    java.util.Objects.requireNonNull((pi));
    java.util.Objects.requireNonNull((radians));
    java.util.Objects.requireNonNull((sin));
    java.util.Objects.requireNonNull((tan));
    this.acos = acos;
    this.asin = asin;
    this.atan = atan;
    this.atan2 = atan2;
    this.cos = cos;
    this.cot = cot;
    this.degrees = degrees;
    this.haversin = haversin;
    this.pi = pi;
    this.radians = radians;
    this.sin = sin;
    this.tan = tan;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TrigonometricFunctionFeatures)) {
      return false;
    }
    TrigonometricFunctionFeatures o = (TrigonometricFunctionFeatures) (other);
    return acos.equals(o.acos) && asin.equals(o.asin) && atan.equals(o.atan) && atan2.equals(o.atan2) && cos.equals(o.cos) && cot.equals(o.cot) && degrees.equals(o.degrees) && haversin.equals(o.haversin) && pi.equals(o.pi) && radians.equals(o.radians) && sin.equals(o.sin) && tan.equals(o.tan);
  }
  
  @Override
  public int hashCode() {
    return 2 * acos.hashCode() + 3 * asin.hashCode() + 5 * atan.hashCode() + 7 * atan2.hashCode() + 11 * cos.hashCode() + 13 * cot.hashCode() + 17 * degrees.hashCode() + 19 * haversin.hashCode() + 23 * pi.hashCode() + 29 * radians.hashCode() + 31 * sin.hashCode() + 37 * tan.hashCode();
  }
  
  public TrigonometricFunctionFeatures withAcos(Boolean acos) {
    java.util.Objects.requireNonNull((acos));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withAsin(Boolean asin) {
    java.util.Objects.requireNonNull((asin));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withAtan(Boolean atan) {
    java.util.Objects.requireNonNull((atan));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withAtan2(Boolean atan2) {
    java.util.Objects.requireNonNull((atan2));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withCos(Boolean cos) {
    java.util.Objects.requireNonNull((cos));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withCot(Boolean cot) {
    java.util.Objects.requireNonNull((cot));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withDegrees(Boolean degrees) {
    java.util.Objects.requireNonNull((degrees));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withHaversin(Boolean haversin) {
    java.util.Objects.requireNonNull((haversin));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withPi(Boolean pi) {
    java.util.Objects.requireNonNull((pi));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withRadians(Boolean radians) {
    java.util.Objects.requireNonNull((radians));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withSin(Boolean sin) {
    java.util.Objects.requireNonNull((sin));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
  
  public TrigonometricFunctionFeatures withTan(Boolean tan) {
    java.util.Objects.requireNonNull((tan));
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
}