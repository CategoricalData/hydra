// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Arithmetic operations
 */
public class ArithmeticFeatures implements Serializable, Comparable<ArithmeticFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.ArithmeticFeatures");
  
  public static final hydra.core.Name PLUS = new hydra.core.Name("plus");
  
  public static final hydra.core.Name MINUS = new hydra.core.Name("minus");
  
  public static final hydra.core.Name MULTIPLY = new hydra.core.Name("multiply");
  
  public static final hydra.core.Name DIVIDE = new hydra.core.Name("divide");
  
  public static final hydra.core.Name MODULUS = new hydra.core.Name("modulus");
  
  public static final hydra.core.Name POWER_OF = new hydra.core.Name("powerOf");
  
  /**
   * The + operator
   */
  public final Boolean plus;
  
  /**
   * The - operator
   */
  public final Boolean minus;
  
  /**
   * The * operator
   */
  public final Boolean multiply;
  
  /**
   * The / operator
   */
  public final Boolean divide;
  
  /**
   * The % operator
   */
  public final Boolean modulus;
  
  /**
   * The ^ operator
   */
  public final Boolean powerOf;
  
  public ArithmeticFeatures (Boolean plus, Boolean minus, Boolean multiply, Boolean divide, Boolean modulus, Boolean powerOf) {
    this.plus = plus;
    this.minus = minus;
    this.multiply = multiply;
    this.divide = divide;
    this.modulus = modulus;
    this.powerOf = powerOf;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArithmeticFeatures)) {
      return false;
    }
    ArithmeticFeatures o = (ArithmeticFeatures) other;
    return java.util.Objects.equals(
      this.plus,
      o.plus) && java.util.Objects.equals(
      this.minus,
      o.minus) && java.util.Objects.equals(
      this.multiply,
      o.multiply) && java.util.Objects.equals(
      this.divide,
      o.divide) && java.util.Objects.equals(
      this.modulus,
      o.modulus) && java.util.Objects.equals(
      this.powerOf,
      o.powerOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(plus) + 3 * java.util.Objects.hashCode(minus) + 5 * java.util.Objects.hashCode(multiply) + 7 * java.util.Objects.hashCode(divide) + 11 * java.util.Objects.hashCode(modulus) + 13 * java.util.Objects.hashCode(powerOf);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ArithmeticFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) plus).compareTo(other.plus);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) minus).compareTo(other.minus);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) multiply).compareTo(other.multiply);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) divide).compareTo(other.divide);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) modulus).compareTo(other.modulus);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) powerOf).compareTo(other.powerOf);
  }
  
  public ArithmeticFeatures withPlus(Boolean plus) {
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withMinus(Boolean minus) {
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withMultiply(Boolean multiply) {
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withDivide(Boolean divide) {
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withModulus(Boolean modulus) {
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withPowerOf(Boolean powerOf) {
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
}
