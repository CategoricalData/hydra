// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class StringLiteral1 implements Serializable, Comparable<StringLiteral1> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral1");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.StringLiteral1_Elmt> value;

  public StringLiteral1 (hydra.util.ConsList<hydra.ext.io.shex.syntax.StringLiteral1_Elmt> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteral1)) {
      return false;
    }
    StringLiteral1 o = (StringLiteral1) other;
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
  public int compareTo(StringLiteral1 other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
