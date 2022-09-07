package hydra.ext.scala.meta;

public class Pat_ExtractInfix {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Pat.ExtractInfix");
  
  public final hydra.ext.scala.meta.Pat lhs;
  
  public final hydra.ext.scala.meta.Data_Name op;
  
  public final java.util.List<hydra.ext.scala.meta.Pat> rhs;
  
  public Pat_ExtractInfix (hydra.ext.scala.meta.Pat lhs, hydra.ext.scala.meta.Data_Name op, java.util.List<hydra.ext.scala.meta.Pat> rhs) {
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_ExtractInfix)) {
      return false;
    }
    Pat_ExtractInfix o = (Pat_ExtractInfix) (other);
    return lhs.equals(o.lhs) && op.equals(o.op) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * op.hashCode() + 5 * rhs.hashCode();
  }
  
  public Pat_ExtractInfix withLhs(hydra.ext.scala.meta.Pat lhs) {
    return new Pat_ExtractInfix(lhs, op, rhs);
  }
  
  public Pat_ExtractInfix withOp(hydra.ext.scala.meta.Data_Name op) {
    return new Pat_ExtractInfix(lhs, op, rhs);
  }
  
  public Pat_ExtractInfix withRhs(java.util.List<hydra.ext.scala.meta.Pat> rhs) {
    return new Pat_ExtractInfix(lhs, op, rhs);
  }
}