// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class CofixBody implements Serializable, Comparable<CofixBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.CofixBody");

  public static final hydra.core.Name IDENT = new hydra.core.Name("ident");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public final hydra.coq.syntax.Ident ident;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public final hydra.util.Maybe<hydra.coq.syntax.Type> type;

  public final hydra.coq.syntax.Term term;

  public CofixBody (hydra.coq.syntax.Ident ident, java.util.List<hydra.coq.syntax.Binder> binders, hydra.util.Maybe<hydra.coq.syntax.Type> type, hydra.coq.syntax.Term term) {
    this.ident = ident;
    this.binders = binders;
    this.type = type;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CofixBody)) {
      return false;
    }
    CofixBody o = (CofixBody) other;
    return java.util.Objects.equals(
      this.ident,
      o.ident) && java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.term,
      o.term);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ident) + 3 * java.util.Objects.hashCode(binders) + 5 * java.util.Objects.hashCode(type) + 7 * java.util.Objects.hashCode(term);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CofixBody other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      ident,
      other.ident);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      binders,
      other.binders);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      term,
      other.term);
  }

  public CofixBody withIdent(hydra.coq.syntax.Ident ident) {
    return new CofixBody(ident, binders, type, term);
  }

  public CofixBody withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new CofixBody(ident, binders, type, term);
  }

  public CofixBody withType(hydra.util.Maybe<hydra.coq.syntax.Type> type) {
    return new CofixBody(ident, binders, type, term);
  }

  public CofixBody withTerm(hydra.coq.syntax.Term term) {
    return new CofixBody(ident, binders, type, term);
  }
}
