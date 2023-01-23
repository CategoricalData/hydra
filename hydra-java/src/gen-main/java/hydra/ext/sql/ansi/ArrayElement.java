package hydra.ext.sql.ansi;

public class ArrayElement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ArrayElement");
  
  public final hydra.ext.sql.ansi.ValueExpression value;
  
  public ArrayElement (hydra.ext.sql.ansi.ValueExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayElement)) {
      return false;
    }
    ArrayElement o = (ArrayElement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}