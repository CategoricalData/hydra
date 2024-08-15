// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class Assignment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.Assignment");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OP = new hydra.core.Name("op");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.langs.java.syntax.LeftHandSide lhs;
  
  public final hydra.langs.java.syntax.AssignmentOperator op;
  
  public final hydra.langs.java.syntax.Expression expression;
  
  public Assignment (hydra.langs.java.syntax.LeftHandSide lhs, hydra.langs.java.syntax.AssignmentOperator op, hydra.langs.java.syntax.Expression expression) {
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
  
  public Assignment withLhs(hydra.langs.java.syntax.LeftHandSide lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Assignment(lhs, op, expression);
  }
  
  public Assignment withOp(hydra.langs.java.syntax.AssignmentOperator op) {
    java.util.Objects.requireNonNull((op));
    return new Assignment(lhs, op, expression);
  }
  
  public Assignment withExpression(hydra.langs.java.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new Assignment(lhs, op, expression);
  }
}