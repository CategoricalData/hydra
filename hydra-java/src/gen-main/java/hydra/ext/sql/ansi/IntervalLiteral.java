package hydra.ext.sql.ansi;

public class IntervalLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.IntervalLiteral");
  
  public IntervalLiteral () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntervalLiteral)) {
      return false;
    }
    IntervalLiteral o = (IntervalLiteral) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}