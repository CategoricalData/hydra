package hydra.ext.coq.syntax;

public class Fix_Decl {
  public final hydra.ext.coq.syntax.Ident ident;
  
  public final java.util.List<hydra.ext.coq.syntax.Binder> binders;
  
  public final java.util.Optional<hydra.ext.coq.syntax.FixAnnot> annot;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Type> type;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public Fix_Decl (hydra.ext.coq.syntax.Ident ident, java.util.List<hydra.ext.coq.syntax.Binder> binders, java.util.Optional<hydra.ext.coq.syntax.FixAnnot> annot, java.util.Optional<hydra.ext.coq.syntax.Type> type, hydra.ext.coq.syntax.Term term) {
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
  
  public Fix_Decl withIdent(hydra.ext.coq.syntax.Ident ident) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withBinders(java.util.List<hydra.ext.coq.syntax.Binder> binders) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withAnnot(java.util.Optional<hydra.ext.coq.syntax.FixAnnot> annot) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withType(java.util.Optional<hydra.ext.coq.syntax.Type> type) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withTerm(hydra.ext.coq.syntax.Term term) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
}