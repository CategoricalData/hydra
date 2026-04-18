// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class FixWith implements Serializable, Comparable<FixWith> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.FixWith");

  public static final hydra.core.Name DECLS = new hydra.core.Name("decls");

  public static final hydra.core.Name FOR = new hydra.core.Name("for");

  public final java.util.List<hydra.coq.syntax.Fix_Decl> decls;

  public final hydra.util.Maybe<hydra.coq.syntax.Ident> for_;

  public FixWith (java.util.List<hydra.coq.syntax.Fix_Decl> decls, hydra.util.Maybe<hydra.coq.syntax.Ident> for_) {
    this.decls = decls;
    this.for_ = for_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixWith)) {
      return false;
    }
    FixWith o = (FixWith) other;
    return java.util.Objects.equals(
      this.decls,
      o.decls) && java.util.Objects.equals(
      this.for_,
      o.for_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(decls) + 3 * java.util.Objects.hashCode(for_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FixWith other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      decls,
      other.decls);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      for_,
      other.for_);
  }

  public FixWith withDecls(java.util.List<hydra.coq.syntax.Fix_Decl> decls) {
    return new FixWith(decls, for_);
  }

  public FixWith withFor(hydra.util.Maybe<hydra.coq.syntax.Ident> for_) {
    return new FixWith(decls, for_);
  }
}
