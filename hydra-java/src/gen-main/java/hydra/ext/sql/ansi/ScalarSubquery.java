package hydra.ext.sql.ansi;

public class ScalarSubquery {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ScalarSubquery");
  
  public final hydra.ext.sql.ansi.Subquery value;
  
  public ScalarSubquery (hydra.ext.sql.ansi.Subquery value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScalarSubquery)) {
      return false;
    }
    ScalarSubquery o = (ScalarSubquery) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}