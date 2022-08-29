package hydra.ext.coq.syntax;

public class CofixWith {
  public final java.util.List<hydra.ext.coq.syntax.CofixBody> with;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Ident> for_;
  
  public CofixWith (java.util.List<hydra.ext.coq.syntax.CofixBody> with, java.util.Optional<hydra.ext.coq.syntax.Ident> for_) {
    this.with = with;
    this.for_ = for_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CofixWith)) {
      return false;
    }
    CofixWith o = (CofixWith) (other);
    return with.equals(o.with) && for_.equals(o.for_);
  }
  
  @Override
  public int hashCode() {
    return 2 * with.hashCode() + 3 * for_.hashCode();
  }
  
  public CofixWith withWith(java.util.List<hydra.ext.coq.syntax.CofixBody> with) {
    return new CofixWith(with, for_);
  }
  
  public CofixWith withFor(java.util.Optional<hydra.ext.coq.syntax.Ident> for_) {
    return new CofixWith(with, for_);
  }
}