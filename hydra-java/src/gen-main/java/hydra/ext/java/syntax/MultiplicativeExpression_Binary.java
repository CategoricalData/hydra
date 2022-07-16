package hydra.ext.java.syntax;

public class MultiplicativeExpression_Binary {
  public final MultiplicativeExpression lhs;
  
  public final UnaryExpression rhs;
  
  public MultiplicativeExpression_Binary (MultiplicativeExpression lhs, UnaryExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiplicativeExpression_Binary)) {
      return false;
    }
    MultiplicativeExpression_Binary o = (MultiplicativeExpression_Binary) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public MultiplicativeExpression_Binary withLhs(MultiplicativeExpression lhs) {
    return new MultiplicativeExpression_Binary(lhs, rhs);
  }
  
  public MultiplicativeExpression_Binary withRhs(UnaryExpression rhs) {
    return new MultiplicativeExpression_Binary(lhs, rhs);
  }
}