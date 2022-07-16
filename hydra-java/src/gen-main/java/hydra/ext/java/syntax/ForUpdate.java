package hydra.ext.java.syntax;

/**
 * Note: list cannot be empty
 */
public class ForUpdate {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<StatementExpression> value;
  
  public ForUpdate (java.util.List<StatementExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForUpdate)) {
      return false;
    }
    ForUpdate o = (ForUpdate) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}