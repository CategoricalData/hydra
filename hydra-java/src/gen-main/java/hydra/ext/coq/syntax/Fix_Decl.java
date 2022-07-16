package hydra.ext.coq.syntax;

public class Fix_Decl {
  public final Ident ident;
  
  public final java.util.List<Binder> binders;
  
  public final java.util.Optional<FixAnnot> annot;
  
  public final java.util.Optional<Type> type;
  
  public final Term term;
  
  public Fix_Decl (Ident ident, java.util.List<Binder> binders, java.util.Optional<FixAnnot> annot, java.util.Optional<Type> type, Term term) {
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
  
  public Fix_Decl withIdent(Ident ident) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withBinders(java.util.List<Binder> binders) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withAnnot(java.util.Optional<FixAnnot> annot) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withType(java.util.Optional<Type> type) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
  
  public Fix_Decl withTerm(Term term) {
    return new Fix_Decl(ident, binders, annot, type, term);
  }
}