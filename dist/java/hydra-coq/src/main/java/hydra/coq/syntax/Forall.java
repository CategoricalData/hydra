// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Forall implements Serializable, Comparable<Forall> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Forall");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public final hydra.coq.syntax.OpenBinders binders;

  public final hydra.coq.syntax.Type type;

  public Forall (hydra.coq.syntax.OpenBinders binders, hydra.coq.syntax.Type type) {
    this.binders = binders;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Forall)) {
      return false;
    }
    Forall o = (Forall) other;
    return java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(binders) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Forall other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      binders,
      other.binders);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public Forall withBinders(hydra.coq.syntax.OpenBinders binders) {
    return new Forall(binders, type);
  }

  public Forall withType(hydra.coq.syntax.Type type) {
    return new Forall(binders, type);
  }
}
