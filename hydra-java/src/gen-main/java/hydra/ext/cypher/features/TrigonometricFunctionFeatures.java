// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Trigonometric functions
 */
public class TrigonometricFunctionFeatures implements Serializable, Comparable<TrigonometricFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.TrigonometricFunctionFeatures");

  public static final hydra.core.Name ACOS = new hydra.core.Name("acos");

  public static final hydra.core.Name ASIN = new hydra.core.Name("asin");

  public static final hydra.core.Name ATAN = new hydra.core.Name("atan");

  public static final hydra.core.Name ATAN2 = new hydra.core.Name("atan2");

  public static final hydra.core.Name COS = new hydra.core.Name("cos");

  public static final hydra.core.Name COT = new hydra.core.Name("cot");

  public static final hydra.core.Name DEGREES = new hydra.core.Name("degrees");

  public static final hydra.core.Name HAVERSIN = new hydra.core.Name("haversin");

  public static final hydra.core.Name PI = new hydra.core.Name("pi");

  public static final hydra.core.Name RADIANS = new hydra.core.Name("radians");

  public static final hydra.core.Name SIN = new hydra.core.Name("sin");

  public static final hydra.core.Name TAN = new hydra.core.Name("tan");

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
    TrigonometricFunctionFeatures o = (TrigonometricFunctionFeatures) other;
    return java.util.Objects.equals(
      this.acos,
      o.acos) && java.util.Objects.equals(
      this.asin,
      o.asin) && java.util.Objects.equals(
      this.atan,
      o.atan) && java.util.Objects.equals(
      this.atan2,
      o.atan2) && java.util.Objects.equals(
      this.cos,
      o.cos) && java.util.Objects.equals(
      this.cot,
      o.cot) && java.util.Objects.equals(
      this.degrees,
      o.degrees) && java.util.Objects.equals(
      this.haversin,
      o.haversin) && java.util.Objects.equals(
      this.pi,
      o.pi) && java.util.Objects.equals(
      this.radians,
      o.radians) && java.util.Objects.equals(
      this.sin,
      o.sin) && java.util.Objects.equals(
      this.tan,
      o.tan);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(acos) + 3 * java.util.Objects.hashCode(asin) + 5 * java.util.Objects.hashCode(atan) + 7 * java.util.Objects.hashCode(atan2) + 11 * java.util.Objects.hashCode(cos) + 13 * java.util.Objects.hashCode(cot) + 17 * java.util.Objects.hashCode(degrees) + 19 * java.util.Objects.hashCode(haversin) + 23 * java.util.Objects.hashCode(pi) + 29 * java.util.Objects.hashCode(radians) + 31 * java.util.Objects.hashCode(sin) + 37 * java.util.Objects.hashCode(tan);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TrigonometricFunctionFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      acos,
      other.acos);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      asin,
      other.asin);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      atan,
      other.atan);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      atan2,
      other.atan2);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      cos,
      other.cos);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      cot,
      other.cot);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      degrees,
      other.degrees);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      haversin,
      other.haversin);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      pi,
      other.pi);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      radians,
      other.radians);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      sin,
      other.sin);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      tan,
      other.tan);
  }

  public TrigonometricFunctionFeatures withAcos(Boolean acos) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withAsin(Boolean asin) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withAtan(Boolean atan) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withAtan2(Boolean atan2) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withCos(Boolean cos) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withCot(Boolean cot) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withDegrees(Boolean degrees) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withHaversin(Boolean haversin) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withPi(Boolean pi) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withRadians(Boolean radians) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withSin(Boolean sin) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }

  public TrigonometricFunctionFeatures withTan(Boolean tan) {
    return new TrigonometricFunctionFeatures(acos, asin, atan, atan2, cos, cot, degrees, haversin, pi, radians, sin, tan);
  }
}
