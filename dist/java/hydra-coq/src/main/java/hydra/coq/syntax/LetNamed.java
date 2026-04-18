// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class LetNamed implements Serializable, Comparable<LetNamed> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.LetNamed");

  public static final hydra.core.Name BINDER = new hydra.core.Name("binder");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public final hydra.coq.syntax.LetBinder binder;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public LetNamed (hydra.coq.syntax.LetBinder binder, java.util.List<hydra.coq.syntax.Binder> binders) {
    this.binder = binder;
    this.binders = binders;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetNamed)) {
      return false;
    }
    LetNamed o = (LetNamed) other;
    return java.util.Objects.equals(
      this.binder,
      o.binder) && java.util.Objects.equals(
      this.binders,
      o.binders);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(binder) + 3 * java.util.Objects.hashCode(binders);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LetNamed other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      binder,
      other.binder);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      binders,
      other.binders);
  }

  public LetNamed withBinder(hydra.coq.syntax.LetBinder binder) {
    return new LetNamed(binder, binders);
  }

  public LetNamed withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new LetNamed(binder, binders);
  }
}
