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
    if (plus == null) {
      throw new IllegalArgumentException("null value for 'plus' argument");
    }
    if (minus == null) {
      throw new IllegalArgumentException("null value for 'minus' argument");
    }
    if (multiply == null) {
      throw new IllegalArgumentException("null value for 'multiply' argument");
    }
    if (divide == null) {
      throw new IllegalArgumentException("null value for 'divide' argument");
    }
    if (modulus == null) {
      throw new IllegalArgumentException("null value for 'modulus' argument");
    }
    if (powerOf == null) {
      throw new IllegalArgumentException("null value for 'powerOf' argument");
    }
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
    if (plus == null) {
      throw new IllegalArgumentException("null value for 'plus' argument");
    }
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withMinus(Boolean minus) {
    if (minus == null) {
      throw new IllegalArgumentException("null value for 'minus' argument");
    }
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withMultiply(Boolean multiply) {
    if (multiply == null) {
      throw new IllegalArgumentException("null value for 'multiply' argument");
    }
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withDivide(Boolean divide) {
    if (divide == null) {
      throw new IllegalArgumentException("null value for 'divide' argument");
    }
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withModulus(Boolean modulus) {
    if (modulus == null) {
      throw new IllegalArgumentException("null value for 'modulus' argument");
    }
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
  
  public ArithmeticFeatures withPowerOf(Boolean powerOf) {
    if (powerOf == null) {
      throw new IllegalArgumentException("null value for 'powerOf' argument");
    }
    return new ArithmeticFeatures(plus, minus, multiply, divide, modulus, powerOf);
  }
}