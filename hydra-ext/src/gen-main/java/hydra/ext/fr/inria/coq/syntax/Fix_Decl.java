// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Fix_Decl implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Fix.Decl");
  
  public static final hydra.core.Name FIELD_NAME_IDENT = new hydra.core.Name("ident");
  
  public static final hydra.core.Name FIELD_NAME_BINDERS = new hydra.core.Name("binders");
  
  public static final hydra.core.Name FIELD_NAME_ANNOT = new hydra.core.Name("annot");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final hydra.ext.fr.inria.coq.syntax.Ident ident;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.FixAnnot> annot;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public Fix_Decl (hydra.ext.fr.inria.coq.syntax.Ident ident, java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.FixAnnot> annot, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type, hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((ident));
    java.util.Objects.requireNonNull((binders));
    java.util.Objects.requireNonNull((annot));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((term));
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
    Fix_Decl o = (Fix_Decl) (other);
    return ident.equals(o.ident) && binders.equals(o.binders) && annot.equals(o.annot) && type.equals(o.type) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * ident.hashCode() + 3 * binders.hashCode() + 5 * annot.hashCode() + 7 * type.hashCode() + 11 * term.hashCode();
  }
  
  public Fix_Decl withIdent(hydra.ext.fr.inria.coq.syntax.Ident ident) {
    java.util.Objects.requireNonNull((ident));
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withBinders(java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders) {
    java.util.Objects.requireNonNull((binders));
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withAnnot(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.FixAnnot> annot) {
    java.util.Objects.requireNonNull((annot));
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withType(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type) {
    java.util.Objects.requireNonNull((type));
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new Fix_Decl(ident, binders, annot, type, term);
  }
}