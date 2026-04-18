// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class FieldIdent implements Serializable, Comparable<FieldIdent> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.FieldIdent");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.coq.syntax.Ident value;

  public FieldIdent (hydra.coq.syntax.Ident value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldIdent)) {
      return false;
    }
    FieldIdent o = (FieldIdent) other;
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
  public int compareTo(FieldIdent other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
