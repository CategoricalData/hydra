package hydra.langs.sql.ansi;

public class LikeClause {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.LikeClause");
  
  public LikeClause () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LikeClause)) {
      return false;
    }
    LikeClause o = (LikeClause) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}