package hydra.ext.sql.ansi;

public class NextValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.NextValueExpression");
  
  public NextValueExpression () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NextValueExpression)) {
      return false;
    }
    NextValueExpression o = (NextValueExpression) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}