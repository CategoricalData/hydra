package hydra.ext.sql.ansi;

public class Subquery {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.Subquery");
  
  public final hydra.ext.sql.ansi.QueryExpression value;
  
  public Subquery (hydra.ext.sql.ansi.QueryExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Subquery)) {
      return false;
    }
    Subquery o = (Subquery) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}