package hydra.ext.sql.ansi;

public class StringValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.StringValueExpression");
  
  public StringValueExpression () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringValueExpression)) {
      return false;
    }
    StringValueExpression o = (StringValueExpression) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}