package hydra.langs.sql.ansi;

public class IntervalValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.IntervalValueExpression");
  
  public IntervalValueExpression () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntervalValueExpression)) {
      return false;
    }
    IntervalValueExpression o = (IntervalValueExpression) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}