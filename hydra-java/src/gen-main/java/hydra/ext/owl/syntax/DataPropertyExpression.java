package hydra.ext.owl.syntax;

public class DataPropertyExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataPropertyExpression");
  
  public final hydra.ext.owl.syntax.DataProperty value;
  
  public DataPropertyExpression (hydra.ext.owl.syntax.DataProperty value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataPropertyExpression)) {
      return false;
    }
    DataPropertyExpression o = (DataPropertyExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}