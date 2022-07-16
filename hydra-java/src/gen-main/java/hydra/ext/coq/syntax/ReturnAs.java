package hydra.ext.coq.syntax;

public class ReturnAs {
  public final java.util.Optional<Name> as;
  
  public final Term100 return_;
  
  public ReturnAs (java.util.Optional<Name> as, Term100 return_) {
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
  
  public ReturnAs withAs(java.util.Optional<Name> as) {
    return new ReturnAs(as, return_);
  }
  
  public ReturnAs withReturn(Term100 return_) {
    return new ReturnAs(as, return_);
  }
}