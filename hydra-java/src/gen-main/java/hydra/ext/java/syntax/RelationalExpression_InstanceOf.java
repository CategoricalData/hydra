package hydra.ext.java.syntax;

public class RelationalExpression_InstanceOf {
  public final RelationalExpression lhs;
  
  public final ReferenceType rhs;
  
  public RelationalExpression_InstanceOf (RelationalExpression lhs, ReferenceType rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_InstanceOf)) {
      return false;
    }
    RelationalExpression_InstanceOf o = (RelationalExpression_InstanceOf) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public RelationalExpression_InstanceOf withLhs(RelationalExpression lhs) {
    return new RelationalExpression_InstanceOf(lhs, rhs);
  }
  
  public RelationalExpression_InstanceOf withRhs(ReferenceType rhs) {
    return new RelationalExpression_InstanceOf(lhs, rhs);
  }
}