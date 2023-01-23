package hydra.ext.sql.ansi;

public class MultisetType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.MultisetType");
  
  public final hydra.ext.sql.ansi.DataType value;
  
  public MultisetType (hydra.ext.sql.ansi.DataType value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultisetType)) {
      return false;
    }
    MultisetType o = (MultisetType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}