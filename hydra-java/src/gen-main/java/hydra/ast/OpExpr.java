// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * An operator expression
 */
public class OpExpr implements Serializable, Comparable<OpExpr> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.OpExpr");
  
  public static final hydra.core.Name FIELD_NAME_OP = new hydra.core.Name("op");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  /**
   * The operator
   */
  public final hydra.ast.Op op;
  
  /**
   * The left-hand side operand
   */
  public final hydra.ast.Expr lhs;
  
  /**
   * The right-hand side operand
   */
  public final hydra.ast.Expr rhs;
  
  public OpExpr (hydra.ast.Op op, hydra.ast.Expr lhs, hydra.ast.Expr rhs) {
    this.op = op;
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpExpr)) {
      return false;
    }
    OpExpr o = (OpExpr) other;
    return java.util.Objects.equals(
      this.op,
      o.op) && java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(op) + 3 * java.util.Objects.hashCode(lhs) + 5 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OpExpr other) {
    int cmp = 0;
    cmp = ((Comparable) op).compareTo(other.op);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public OpExpr withOp(hydra.ast.Op op) {
    return new OpExpr(op, lhs, rhs);
  }
  
  public OpExpr withLhs(hydra.ast.Expr lhs) {
    return new OpExpr(op, lhs, rhs);
  }
  
  public OpExpr withRhs(hydra.ast.Expr rhs) {
    return new OpExpr(op, lhs, rhs);
  }
}
