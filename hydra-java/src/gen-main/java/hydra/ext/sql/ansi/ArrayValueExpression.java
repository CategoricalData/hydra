package hydra.ext.sql.ansi;

public class ArrayValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ArrayValueExpression");
  
  public ArrayValueExpression () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayValueExpression)) {
      return false;
    }
    ArrayValueExpression o = (ArrayValueExpression) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}