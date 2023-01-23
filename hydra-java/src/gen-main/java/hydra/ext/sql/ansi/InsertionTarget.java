package hydra.ext.sql.ansi;

public class InsertionTarget {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.InsertionTarget");
  
  public final hydra.ext.sql.ansi.TableName value;
  
  public InsertionTarget (hydra.ext.sql.ansi.TableName value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InsertionTarget)) {
      return false;
    }
    InsertionTarget o = (InsertionTarget) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}