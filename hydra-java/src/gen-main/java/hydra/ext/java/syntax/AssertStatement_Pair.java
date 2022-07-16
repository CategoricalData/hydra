package hydra.ext.java.syntax;

public class AssertStatement_Pair {
  public final Expression first;
  
  public final Expression second;
  
  public AssertStatement_Pair (Expression first, Expression second) {
    this.first = first;
    this.second = second;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssertStatement_Pair)) {
      return false;
    }
    AssertStatement_Pair o = (AssertStatement_Pair) (other);
    return first.equals(o.first) && second.equals(o.second);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * second.hashCode();
  }
  
  public AssertStatement_Pair withFirst(Expression first) {
    return new AssertStatement_Pair(first, second);
  }
  
  public AssertStatement_Pair withSecond(Expression second) {
    return new AssertStatement_Pair(first, second);
  }
}