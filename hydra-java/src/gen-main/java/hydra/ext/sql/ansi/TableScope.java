package hydra.ext.sql.ansi;

public class TableScope {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.TableScope");
  
  public final hydra.ext.sql.ansi.GlobalOrLocal value;
  
  public TableScope (hydra.ext.sql.ansi.GlobalOrLocal value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TableScope)) {
      return false;
    }
    TableScope o = (TableScope) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}