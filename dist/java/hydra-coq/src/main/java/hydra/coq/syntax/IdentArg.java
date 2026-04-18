// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class IdentArg implements Serializable, Comparable<IdentArg> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.IdentArg");

  public static final hydra.core.Name IDENT = new hydra.core.Name("ident");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public final hydra.coq.syntax.Ident ident;

  public final hydra.coq.syntax.Term term;

  public IdentArg (hydra.coq.syntax.Ident ident, hydra.coq.syntax.Term term) {
    this.ident = ident;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IdentArg)) {
      return false;
    }
    IdentArg o = (IdentArg) other;
    return java.util.Objects.equals(
      this.ident,
      o.ident) && java.util.Objects.equals(
      this.term,
      o.term);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ident) + 3 * java.util.Objects.hashCode(term);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IdentArg other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      ident,
      other.ident);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      term,
      other.term);
  }

  public IdentArg withIdent(hydra.coq.syntax.Ident ident) {
    return new IdentArg(ident, term);
  }

  public IdentArg withTerm(hydra.coq.syntax.Term term) {
    return new IdentArg(ident, term);
  }
}
