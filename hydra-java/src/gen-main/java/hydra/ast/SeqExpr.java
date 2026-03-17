// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * A sequence of expressions joined by a separator operator. Unlike OpExpr, parenthesize ignores SeqExpr boundaries.
 */
public class SeqExpr implements Serializable, Comparable<SeqExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ast.SeqExpr");

  public static final hydra.core.Name OP = new hydra.core.Name("op");

  public static final hydra.core.Name ELEMENTS = new hydra.core.Name("elements");

  /**
   * The separator operator
   */
  public final hydra.ast.Op op;

  /**
   * The expressions to join
   */
  public final hydra.util.ConsList<hydra.ast.Expr> elements;

  public SeqExpr (hydra.ast.Op op, hydra.util.ConsList<hydra.ast.Expr> elements) {
    this.op = op;
    this.elements = elements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SeqExpr)) {
      return false;
    }
    SeqExpr o = (SeqExpr) other;
    return java.util.Objects.equals(
      this.op,
      o.op) && java.util.Objects.equals(
      this.elements,
      o.elements);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(op) + 3 * java.util.Objects.hashCode(elements);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SeqExpr other) {
    int cmp = 0;
    cmp = ((Comparable) op).compareTo(other.op);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) elements).compareTo(other.elements);
  }

  public SeqExpr withOp(hydra.ast.Op op) {
    return new SeqExpr(op, elements);
  }

  public SeqExpr withElements(hydra.util.ConsList<hydra.ast.Expr> elements) {
    return new SeqExpr(op, elements);
  }
}
