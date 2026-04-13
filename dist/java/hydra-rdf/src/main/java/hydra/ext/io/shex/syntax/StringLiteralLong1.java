// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class StringLiteralLong1 implements Serializable, Comparable<StringLiteralLong1> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt> value;

  public StringLiteralLong1 (java.util.List<hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteralLong1)) {
      return false;
    }
    StringLiteralLong1 o = (StringLiteralLong1) other;
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
  public int compareTo(StringLiteralLong1 other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
