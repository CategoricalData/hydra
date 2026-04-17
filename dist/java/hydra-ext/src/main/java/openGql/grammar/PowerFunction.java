// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PowerFunction implements Serializable, Comparable<PowerFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PowerFunction");

  public static final hydra.core.Name BASE = new hydra.core.Name("base");

  public static final hydra.core.Name EXPONENT = new hydra.core.Name("exponent");

  public final openGql.grammar.NumericValueExpression base;

  public final openGql.grammar.NumericValueExpression exponent;

  public PowerFunction (openGql.grammar.NumericValueExpression base, openGql.grammar.NumericValueExpression exponent) {
    this.base = base;
    this.exponent = exponent;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PowerFunction)) {
      return false;
    }
    PowerFunction o = (PowerFunction) other;
    return java.util.Objects.equals(
      this.base,
      o.base) && java.util.Objects.equals(
      this.exponent,
      o.exponent);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(base) + 3 * java.util.Objects.hashCode(exponent);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PowerFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      base,
      other.base);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      exponent,
      other.exponent);
  }

  public PowerFunction withBase(openGql.grammar.NumericValueExpression base) {
    return new PowerFunction(base, exponent);
  }

  public PowerFunction withExponent(openGql.grammar.NumericValueExpression exponent) {
    return new PowerFunction(base, exponent);
  }
}
