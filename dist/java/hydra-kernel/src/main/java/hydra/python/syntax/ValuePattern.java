// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ValuePattern implements Serializable, Comparable<ValuePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ValuePattern");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.python.syntax.Attribute value;

  public ValuePattern (hydra.python.syntax.Attribute value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValuePattern)) {
      return false;
    }
    ValuePattern o = (ValuePattern) other;
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
  public int compareTo(ValuePattern other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
