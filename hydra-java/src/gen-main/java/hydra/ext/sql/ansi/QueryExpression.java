package hydra.ext.sql.ansi;

public class QueryExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.QueryExpression");
  
  public QueryExpression () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QueryExpression)) {
      return false;
    }
    QueryExpression o = (QueryExpression) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}