// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ValueSet implements Serializable, Comparable<ValueSet> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ValueSet");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.ValueSetValue> value;

  public ValueSet (hydra.util.ConsList<hydra.ext.io.shex.syntax.ValueSetValue> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValueSet)) {
      return false;
    }
    ValueSet o = (ValueSet) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ValueSet other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
