package hydra.ext.java.syntax;

public class Assignment {
  public final LeftHandSide lhs;
  
  public final AssignmentOperator op;
  
  public final Expression expression;
  
  public Assignment (LeftHandSide lhs, AssignmentOperator op, Expression expression) {
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
  
  public Assignment withLhs(LeftHandSide lhs) {
    return new Assignment(lhs, op, expression);
  }
  
  public Assignment withOp(AssignmentOperator op) {
    return new Assignment(lhs, op, expression);
  }
  
  public Assignment withExpression(Expression expression) {
    return new Assignment(lhs, op, expression);
  }
}