// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Fix_Decl implements Serializable, Comparable<Fix_Decl> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Fix_Decl");

  public static final hydra.core.Name IDENT = new hydra.core.Name("ident");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  public static final hydra.core.Name ANNOT = new hydra.core.Name("annot");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public final hydra.coq.syntax.Ident ident;

  public final java.util.List<hydra.coq.syntax.Binder> binders;

  public final hydra.util.Maybe<hydra.coq.syntax.FixAnnot> annot;

  public final hydra.util.Maybe<hydra.coq.syntax.Type> type;

  public final hydra.coq.syntax.Term term;

  public Fix_Decl (hydra.coq.syntax.Ident ident, java.util.List<hydra.coq.syntax.Binder> binders, hydra.util.Maybe<hydra.coq.syntax.FixAnnot> annot, hydra.util.Maybe<hydra.coq.syntax.Type> type, hydra.coq.syntax.Term term) {
    this.ident = ident;
    this.binders = binders;
    this.annot = annot;
    this.type = type;
    this.term = term;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fix_Decl)) {
      return false;
    }
    Fix_Decl o = (Fix_Decl) other;
    return java.util.Objects.equals(
      this.ident,
      o.ident) && java.util.Objects.equals(
      this.binders,
      o.binders) && java.util.Objects.equals(
      this.annot,
      o.annot) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.term,
      o.term);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ident) + 3 * java.util.Objects.hashCode(binders) + 5 * java.util.Objects.hashCode(annot) + 7 * java.util.Objects.hashCode(type) + 11 * java.util.Objects.hashCode(term);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Fix_Decl other) {
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
      annot,
      other.annot);
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

  public Fix_Decl withIdent(hydra.coq.syntax.Ident ident) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }

  public Fix_Decl withBinders(java.util.List<hydra.coq.syntax.Binder> binders) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }

  public Fix_Decl withAnnot(hydra.util.Maybe<hydra.coq.syntax.FixAnnot> annot) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }

  public Fix_Decl withType(hydra.util.Maybe<hydra.coq.syntax.Type> type) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }

  public Fix_Decl withTerm(hydra.coq.syntax.Term term) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
}
