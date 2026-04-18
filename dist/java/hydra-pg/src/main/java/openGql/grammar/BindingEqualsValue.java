// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class BindingEqualsValue implements Serializable, Comparable<BindingEqualsValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BindingEqualsValue");

  public static final hydra.core.Name BINDING = new hydra.core.Name("binding");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String binding;

  public final openGql.grammar.ValueExpression value;

  public BindingEqualsValue (String binding, openGql.grammar.ValueExpression value) {
    this.binding = binding;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BindingEqualsValue)) {
      return false;
    }
    BindingEqualsValue o = (BindingEqualsValue) other;
    return java.util.Objects.equals(
      this.binding,
      o.binding) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(binding) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BindingEqualsValue other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      binding,
      other.binding);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public BindingEqualsValue withBinding(String binding) {
    return new BindingEqualsValue(binding, value);
  }

  public BindingEqualsValue withValue(openGql.grammar.ValueExpression value) {
    return new BindingEqualsValue(binding, value);
  }
}
