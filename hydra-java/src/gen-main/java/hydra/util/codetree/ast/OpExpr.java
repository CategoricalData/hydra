package hydra.util.codetree.ast;

/**
 * An operator expression
 */
public class OpExpr {
  public final hydra.util.codetree.ast.Op op;
  
  public final hydra.util.codetree.ast.Expr lhs;
  
  public final hydra.util.codetree.ast.Expr rhs;
  
  public OpExpr (hydra.util.codetree.ast.Op op, hydra.util.codetree.ast.Expr lhs, hydra.util.codetree.ast.Expr rhs) {
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
  
  public OpExpr withOp(hydra.util.codetree.ast.Op op) {
    return new OpExpr(op, lhs, rhs);
  }
  
  public OpExpr withLhs(hydra.util.codetree.ast.Expr lhs) {
    return new OpExpr(op, lhs, rhs);
  }
  
  public OpExpr withRhs(hydra.util.codetree.ast.Expr rhs) {
    return new OpExpr(op, lhs, rhs);
  }
}