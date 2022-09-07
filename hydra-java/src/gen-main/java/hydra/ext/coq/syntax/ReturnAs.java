package hydra.ext.coq.syntax;

public class ReturnAs {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.ReturnAs");
  
  public final java.util.Optional<hydra.ext.coq.syntax.Name> as;
  
  public final hydra.ext.coq.syntax.Term100 return_;
  
  public ReturnAs (java.util.Optional<hydra.ext.coq.syntax.Name> as, hydra.ext.coq.syntax.Term100 return_) {
    this.as = as;
    this.return_ = return_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReturnAs)) {
      return false;
    }
    ReturnAs o = (ReturnAs) (other);
    return as.equals(o.as) && return_.equals(o.return_);
  }
  
  @Override
  public int hashCode() {
    return 2 * as.hashCode() + 3 * return_.hashCode();
  }
  
  public ReturnAs withAs(java.util.Optional<hydra.ext.coq.syntax.Name> as) {
    return new ReturnAs(as, return_);
  }
  
  public ReturnAs withReturn(hydra.ext.coq.syntax.Term100 return_) {
    return new ReturnAs(as, return_);
  }
}