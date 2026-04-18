// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class CofixWith implements Serializable, Comparable<CofixWith> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.CofixWith");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

  public static final hydra.core.Name FOR = new hydra.core.Name("for");

  public final java.util.List<hydra.coq.syntax.CofixBody> with;

  public final hydra.util.Maybe<hydra.coq.syntax.Ident> for_;

  public CofixWith (java.util.List<hydra.coq.syntax.CofixBody> with, hydra.util.Maybe<hydra.coq.syntax.Ident> for_) {
    this.with = with;
    this.for_ = for_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CofixWith)) {
      return false;
    }
    CofixWith o = (CofixWith) other;
    return java.util.Objects.equals(
      this.with,
      o.with) && java.util.Objects.equals(
      this.for_,
      o.for_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(with) + 3 * java.util.Objects.hashCode(for_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CofixWith other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      with,
      other.with);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      for_,
      other.for_);
  }

  public CofixWith withWith(java.util.List<hydra.coq.syntax.CofixBody> with) {
    return new CofixWith(with, for_);
  }

  public CofixWith withFor(hydra.util.Maybe<hydra.coq.syntax.Ident> for_) {
    return new CofixWith(with, for_);
  }
}
