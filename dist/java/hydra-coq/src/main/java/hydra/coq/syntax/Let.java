// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A let-in definition
 */
public class Let implements Serializable, Comparable<Let> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Let");

  public static final hydra.core.Name BINDINGS = new hydra.core.Name("bindings");

  public static final hydra.core.Name IN = new hydra.core.Name("in");

  public final hydra.coq.syntax.LetBindings bindings;

  public final hydra.coq.syntax.Term in;

  public Let (hydra.coq.syntax.LetBindings bindings, hydra.coq.syntax.Term in) {
    this.bindings = bindings;
    this.in = in;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Let)) {
      return false;
    }
    Let o = (Let) other;
    return java.util.Objects.equals(
      this.bindings,
      o.bindings) && java.util.Objects.equals(
      this.in,
      o.in);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bindings) + 3 * java.util.Objects.hashCode(in);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Let other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      bindings,
      other.bindings);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      in,
      other.in);
  }

  public Let withBindings(hydra.coq.syntax.LetBindings bindings) {
    return new Let(bindings, in);
  }

  public Let withIn(hydra.coq.syntax.Term in) {
    return new Let(bindings, in);
  }
}
