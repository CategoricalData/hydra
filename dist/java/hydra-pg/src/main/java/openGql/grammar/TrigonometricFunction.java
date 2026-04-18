// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TrigonometricFunction implements Serializable, Comparable<TrigonometricFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TrigonometricFunction");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final openGql.grammar.TrigonometricFunctionName name;

  public final openGql.grammar.NumericValueExpression value;

  public TrigonometricFunction (openGql.grammar.TrigonometricFunctionName name, openGql.grammar.NumericValueExpression value) {
    this.name = name;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TrigonometricFunction)) {
      return false;
    }
    TrigonometricFunction o = (TrigonometricFunction) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TrigonometricFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public TrigonometricFunction withName(openGql.grammar.TrigonometricFunctionName name) {
    return new TrigonometricFunction(name, value);
  }

  public TrigonometricFunction withValue(openGql.grammar.NumericValueExpression value) {
    return new TrigonometricFunction(name, value);
  }
}
