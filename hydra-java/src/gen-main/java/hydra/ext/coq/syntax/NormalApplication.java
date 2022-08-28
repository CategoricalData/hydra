package hydra.ext.coq.syntax;

public class NormalApplication {
  public final hydra.ext.coq.syntax.Term1 lhs;
  
  public final java.util.List<hydra.ext.coq.syntax.Arg> rhs;
  
  public NormalApplication (hydra.ext.coq.syntax.Term1 lhs, java.util.List<hydra.ext.coq.syntax.Arg> rhs) {
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
  
  public NormalApplication withLhs(hydra.ext.coq.syntax.Term1 lhs) {
    return new NormalApplication(lhs, rhs);
  }
  
  public NormalApplication withRhs(java.util.List<hydra.ext.coq.syntax.Arg> rhs) {
    return new NormalApplication(lhs, rhs);
  }
}