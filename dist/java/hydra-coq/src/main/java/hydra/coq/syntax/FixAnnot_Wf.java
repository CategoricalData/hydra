// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class FixAnnot_Wf implements Serializable, Comparable<FixAnnot_Wf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.FixAnnot_Wf");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name IDENT = new hydra.core.Name("ident");

  public final hydra.coq.syntax.OneTerm term;

  public final hydra.coq.syntax.Ident ident;

  public FixAnnot_Wf (hydra.coq.syntax.OneTerm term, hydra.coq.syntax.Ident ident) {
    this.term = term;
    this.ident = ident;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixAnnot_Wf)) {
      return false;
    }
    FixAnnot_Wf o = (FixAnnot_Wf) other;
    return java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.ident,
      o.ident);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(term) + 3 * java.util.Objects.hashCode(ident);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FixAnnot_Wf other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      term,
      other.term);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      ident,
      other.ident);
  }

  public FixAnnot_Wf withTerm(hydra.coq.syntax.OneTerm term) {
    return new FixAnnot_Wf(term, ident);
  }

  public FixAnnot_Wf withIdent(hydra.coq.syntax.Ident ident) {
    return new FixAnnot_Wf(term, ident);
  }
}
