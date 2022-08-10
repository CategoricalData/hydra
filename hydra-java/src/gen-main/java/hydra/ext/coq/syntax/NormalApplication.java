package hydra.ext.coq.syntax;

public class NormalApplication {
  public final Term1 lhs;
  
  public final java.util.List<Arg> rhs;
  
  public NormalApplication (Term1 lhs, java.util.List<Arg> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalApplication)) {
      return false;
    }
    NormalApplication o = (NormalApplication) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public NormalApplication withLhs(Term1 lhs) {
    return new NormalApplication(lhs, rhs);
  }
  
  public NormalApplication withRhs(java.util.List<Arg> rhs) {
    return new NormalApplication(lhs, rhs);
  }
}