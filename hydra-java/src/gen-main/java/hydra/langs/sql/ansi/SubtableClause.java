package hydra.langs.sql.ansi;

public class SubtableClause {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.SubtableClause");
  
  public SubtableClause () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtableClause)) {
      return false;
    }
    SubtableClause o = (SubtableClause) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}