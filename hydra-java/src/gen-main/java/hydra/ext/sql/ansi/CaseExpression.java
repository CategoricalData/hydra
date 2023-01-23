package hydra.ext.sql.ansi;

public class CaseExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.CaseExpression");
  
  public CaseExpression () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseExpression)) {
      return false;
    }
    CaseExpression o = (CaseExpression) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}