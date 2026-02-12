// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class Assignment implements Serializable, Comparable<Assignment> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.Assignment");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OP = new hydra.core.Name("op");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.java.syntax.LeftHandSide lhs;
  
  public final hydra.ext.java.syntax.AssignmentOperator op;
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public Assignment (hydra.ext.java.syntax.LeftHandSide lhs, hydra.ext.java.syntax.AssignmentOperator op, hydra.ext.java.syntax.Expression expression) {
    this.lhs = lhs;
    this.op = op;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Assignment)) {
      return false;
    }
    Assignment o = (Assignment) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.op,
      o.op) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(op) + 5 * java.util.Objects.hashCode(expression);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Assignment other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) op).compareTo(other.op);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }
  
  public Assignment withLhs(hydra.ext.java.syntax.LeftHandSide lhs) {
    return new Assignment(lhs, op, expression);
  }
  
  public Assignment withOp(hydra.ext.java.syntax.AssignmentOperator op) {
    return new Assignment(lhs, op, expression);
  }
  
  public Assignment withExpression(hydra.ext.java.syntax.Expression expression) {
    return new Assignment(lhs, op, expression);
  }
}
