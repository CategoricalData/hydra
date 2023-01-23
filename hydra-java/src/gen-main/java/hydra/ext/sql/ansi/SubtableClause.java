package hydra.ext.sql.ansi;

public class SubtableClause {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.SubtableClause");
  
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