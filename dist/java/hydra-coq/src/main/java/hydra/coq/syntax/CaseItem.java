// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class CaseItem implements Serializable, Comparable<CaseItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.CaseItem");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public static final hydra.core.Name IN = new hydra.core.Name("in");

  public final hydra.coq.syntax.Term100 term;

  public final hydra.util.Maybe<hydra.coq.syntax.Name> as;

  public final hydra.util.Maybe<hydra.coq.syntax.Pattern> in;

  public CaseItem (hydra.coq.syntax.Term100 term, hydra.util.Maybe<hydra.coq.syntax.Name> as, hydra.util.Maybe<hydra.coq.syntax.Pattern> in) {
    this.term = term;
    this.as = as;
    this.in = in;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseItem)) {
      return false;
    }
    CaseItem o = (CaseItem) other;
    return java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.as,
      o.as) && java.util.Objects.equals(
      this.in,
      o.in);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(term) + 3 * java.util.Objects.hashCode(as) + 5 * java.util.Objects.hashCode(in);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseItem other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      term,
      other.term);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      as,
      other.as);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      in,
      other.in);
  }

  public CaseItem withTerm(hydra.coq.syntax.Term100 term) {
    return new CaseItem(term, as, in);
  }

  public CaseItem withAs(hydra.util.Maybe<hydra.coq.syntax.Name> as) {
    return new CaseItem(term, as, in);
  }

  public CaseItem withIn(hydra.util.Maybe<hydra.coq.syntax.Pattern> in) {
    return new CaseItem(term, as, in);
  }
}
