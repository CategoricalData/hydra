// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class StringLiteralLong2 implements Serializable, Comparable<StringLiteralLong2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.StringLiteralLong2");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.shex.syntax.StringLiteralLong2_Elmt> value;

  public StringLiteralLong2 (java.util.List<hydra.shex.syntax.StringLiteralLong2_Elmt> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteralLong2)) {
      return false;
    }
    StringLiteralLong2 o = (StringLiteralLong2) other;
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
  public int compareTo(StringLiteralLong2 other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
