package hydra.util.codetree.ast;

/**
 * An operator expression
 */
public class OpExpr {
  public final Op op;
  
  public final Expr lhs;
  
  public final Expr rhs;
  
  public OpExpr (Op op, Expr lhs, Expr rhs) {
    this.op = op;
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpExpr)) {
      return false;
    }
    OpExpr o = (OpExpr) (other);
    return op.equals(o.op) && lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * op.hashCode() + 3 * lhs.hashCode() + 5 * rhs.hashCode();
  }
  
  public OpExpr withOp(Op op) {
    return new OpExpr(op, lhs, rhs);
  }
  
  public OpExpr withLhs(Expr lhs) {
    return new OpExpr(op, lhs, rhs);
  }
  
  public OpExpr withRhs(Expr rhs) {
    return new OpExpr(op, lhs, rhs);
  }
}