// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Fun implements Serializable, Comparable<Fun> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Fun");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.coq.syntax.OpenBinders binders;

  public final hydra.coq.syntax.Term body;

  public Fun (hydra.coq.syntax.OpenBinders binders, hydra.coq.syntax.Term body) {
    this.binders = binders;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fun)) {
      return false;
    }
    Fun o = (Fun) other;
    return java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(binders) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Fun other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      binders,
      other.binders);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public Fun withBinders(hydra.coq.syntax.OpenBinders binders) {
    return new Fun(binders, body);
  }

  public Fun withBody(hydra.coq.syntax.Term body) {
    return new Fun(binders, body);
  }
}
