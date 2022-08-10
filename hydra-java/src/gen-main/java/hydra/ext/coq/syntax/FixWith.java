package hydra.ext.coq.syntax;

public class FixWith {
  public final java.util.List<Fix_Decl> decls;
  
  public final java.util.Optional<Ident> for_;
  
  public FixWith (java.util.List<Fix_Decl> decls, java.util.Optional<Ident> for_) {
    this.decls = decls;
    this.for_ = for_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixWith)) {
      return false;
    }
    FixWith o = (FixWith) (other);
    return decls.equals(o.decls) && for_.equals(o.for_);
  }
  
  @Override
  public int hashCode() {
    return 2 * decls.hashCode() + 3 * for_.hashCode();
  }
  
  public FixWith withDecls(java.util.List<Fix_Decl> decls) {
    return new FixWith(decls, for_);
  }
  
  public FixWith withFor(java.util.Optional<Ident> for_) {
    return new FixWith(decls, for_);
  }
}