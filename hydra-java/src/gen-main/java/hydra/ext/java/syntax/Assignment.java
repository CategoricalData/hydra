// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class Assignment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.Assignment");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OP = new hydra.core.Name("op");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.java.syntax.LeftHandSide lhs;
  
  public final hydra.ext.java.syntax.AssignmentOperator op;
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public Assignment (hydra.ext.java.syntax.LeftHandSide lhs, hydra.ext.java.syntax.AssignmentOperator op, hydra.ext.java.syntax.Expression expression) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((op));
    java.util.Objects.requireNonNull((expression));
    this.lhs = lhs;
    this.op = op;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Assignment)) {
      return false;
    }
    Assignment o = (Assignment) (other);
    return lhs.equals(o.lhs) && op.equals(o.op) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * op.hashCode() + 5 * expression.hashCode();
  }
  
  public Assignment withLhs(hydra.ext.java.syntax.LeftHandSide lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Assignment(lhs, op, expression);
  }
  
  public Assignment withOp(hydra.ext.java.syntax.AssignmentOperator op) {
    java.util.Objects.requireNonNull((op));
    return new Assignment(lhs, op, expression);
  }
  
  public Assignment withExpression(hydra.ext.java.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new Assignment(lhs, op, expression);
  }
}
