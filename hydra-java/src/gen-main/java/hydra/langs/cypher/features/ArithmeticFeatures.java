// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for arithmetic operations.
 */
public class ArithmeticFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.ArithmeticFeatures");
  
  /**
   * Whether to expect the + operator.
   */
  public final Boolean plus;
  
  /**
   * Whether to expect the - operator.
   */
  public final Boolean minus;
  
  /**
   * Whether to expect the * operator.
   */
  public final Boolean multiply;
  
  /**
   * Whether to expect the / operator.
   */
  public final Boolean divide;
  
  /**
   * Whether to expect the % operator.
   */
  public final Boolean modulus;
  
  /**
   * Whether to expect the ^ operator.
   */
  public final Boolean powerOf;
  
  public ArithmeticFeatures (Boolean plus, Boolean minus, Boolean multiply, Boolean divide, Boolean modulus, Boolean powerOf) {
    java.util.Objects.requireNonNull((plus));
    java.util.Objects.requireNonNull((minus));
    java.util.Objects.requireNonNull((multiply));
    java.util.Objects.requireNonNull((divide));
    java.util.Objects.requireNonNull((modulus));
    java.util.Objects.requireNonNull((powerOf));
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
    ArithmeticFeatures o = (ArithmeticFeatures) (other);
    return plus.equals(o.plus) && minus.equals(o.minus) && multiply.equals(o.multiply) && divide.equals(o.divide) && modulus.equals(o.modulus) && powerOf.equals(o.powerOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * plus.hashCode() + 3 * minus.hashCode() + 5 * multiply.hashCode() + 7 * divide.hashCode() + 11 * modulus.hashCode() + 13 * powerOf.hashCode();
  }
  
  public ArithmeticFeatures withPlus(Boolean plus) {
    java.util.Objects.requireNonNull((plus));
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withMinus(Boolean minus) {
    java.util.Objects.requireNonNull((minus));
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withMultiply(Boolean multiply) {
    java.util.Objects.requireNonNull((multiply));
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withDivide(Boolean divide) {
    java.util.Objects.requireNonNull((divide));
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withModulus(Boolean modulus) {
    java.util.Objects.requireNonNull((modulus));
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withPowerOf(Boolean powerOf) {
    java.util.Objects.requireNonNull((powerOf));
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
}