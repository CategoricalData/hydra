package hydra.langs.sql.ansi;

public class CollateClause {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.CollateClause");
  
  public CollateClause () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CollateClause)) {
      return false;
    }
    CollateClause o = (CollateClause) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}