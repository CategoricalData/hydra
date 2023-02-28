package hydra.langs.sql.ansi;

public class RowType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.RowType");
  
  public RowType () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RowType)) {
      return false;
    }
    RowType o = (RowType) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}