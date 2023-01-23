package hydra.ext.sql.ansi;

public class InsertColumnList {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.InsertColumnList");
  
  public final hydra.ext.sql.ansi.ColumnNameList value;
  
  public InsertColumnList (hydra.ext.sql.ansi.ColumnNameList value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InsertColumnList)) {
      return false;
    }
    InsertColumnList o = (InsertColumnList) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}