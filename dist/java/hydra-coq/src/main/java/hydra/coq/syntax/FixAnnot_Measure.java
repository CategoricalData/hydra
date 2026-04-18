// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class FixAnnot_Measure implements Serializable, Comparable<FixAnnot_Measure> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.FixAnnot_Measure");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name IDENT = new hydra.core.Name("ident");

  public static final hydra.core.Name TERM2 = new hydra.core.Name("term2");

  public final hydra.coq.syntax.OneTerm term;

  public final hydra.util.Maybe<hydra.coq.syntax.Ident> ident;

  public final hydra.util.Maybe<hydra.coq.syntax.OneTerm> term2;

  public FixAnnot_Measure (hydra.coq.syntax.OneTerm term, hydra.util.Maybe<hydra.coq.syntax.Ident> ident, hydra.util.Maybe<hydra.coq.syntax.OneTerm> term2) {
    this.term = term;
    this.ident = ident;
    this.term2 = term2;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixAnnot_Measure)) {
      return false;
    }
    FixAnnot_Measure o = (FixAnnot_Measure) other;
    return java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.ident,
      o.ident) && java.util.Objects.equals(
      this.term2,
      o.term2);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(term) + 3 * java.util.Objects.hashCode(ident) + 5 * java.util.Objects.hashCode(term2);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FixAnnot_Measure other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      term,
      other.term);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      ident,
      other.ident);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      term2,
      other.term2);
  }

  public FixAnnot_Measure withTerm(hydra.coq.syntax.OneTerm term) {
    return new FixAnnot_Measure(term, ident, term2);
  }

  public FixAnnot_Measure withIdent(hydra.util.Maybe<hydra.coq.syntax.Ident> ident) {
    return new FixAnnot_Measure(term, ident, term2);
  }

  public FixAnnot_Measure withTerm2(hydra.util.Maybe<hydra.coq.syntax.OneTerm> term2) {
    return new FixAnnot_Measure(term, ident, term2);
  }
}
