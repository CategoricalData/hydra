// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class ExistentialVariable implements Serializable, Comparable<ExistentialVariable> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.ExistentialVariable");

  public static final hydra.core.Name IDENT = new hydra.core.Name("ident");

  public static final hydra.core.Name VARIANT = new hydra.core.Name("variant");

  public final hydra.coq.syntax.Ident ident;

  public final hydra.coq.syntax.ExistentialVariableVariant variant;

  public ExistentialVariable (hydra.coq.syntax.Ident ident, hydra.coq.syntax.ExistentialVariableVariant variant) {
    this.ident = ident;
    this.variant = variant;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExistentialVariable)) {
      return false;
    }
    ExistentialVariable o = (ExistentialVariable) other;
    return java.util.Objects.equals(
      this.ident,
      o.ident) && java.util.Objects.equals(
      this.variant,
      o.variant);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ident) + 3 * java.util.Objects.hashCode(variant);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExistentialVariable other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      ident,
      other.ident);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      variant,
      other.variant);
  }

  public ExistentialVariable withIdent(hydra.coq.syntax.Ident ident) {
    return new ExistentialVariable(ident, variant);
  }

  public ExistentialVariable withVariant(hydra.coq.syntax.ExistentialVariableVariant variant) {
    return new ExistentialVariable(ident, variant);
  }
}
