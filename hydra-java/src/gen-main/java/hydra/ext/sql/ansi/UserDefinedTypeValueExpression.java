package hydra.ext.sql.ansi;

public class UserDefinedTypeValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.UserDefinedTypeValueExpression");
  
  public final hydra.ext.sql.ansi.ValueExpressionPrimary value;
  
  public UserDefinedTypeValueExpression (hydra.ext.sql.ansi.ValueExpressionPrimary value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UserDefinedTypeValueExpression)) {
      return false;
    }
    UserDefinedTypeValueExpression o = (UserDefinedTypeValueExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}