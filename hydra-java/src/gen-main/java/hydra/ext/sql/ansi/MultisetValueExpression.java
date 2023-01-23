package hydra.ext.sql.ansi;

public class MultisetValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.MultisetValueExpression");
  
  public MultisetValueExpression () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultisetValueExpression)) {
      return false;
    }
    MultisetValueExpression o = (MultisetValueExpression) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}