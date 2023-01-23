package hydra.ext.sql.ansi;

public class ReferenceValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ReferenceValueExpression");
  
  public final hydra.ext.sql.ansi.ValueExpressionPrimary value;
  
  public ReferenceValueExpression (hydra.ext.sql.ansi.ValueExpressionPrimary value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReferenceValueExpression)) {
      return false;
    }
    ReferenceValueExpression o = (ReferenceValueExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}