package hydra.ext.sql.ansi;

public class Scale {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.Scale");
  
  public final hydra.ext.sql.ansi.UnsignedInteger value;
  
  public Scale (hydra.ext.sql.ansi.UnsignedInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Scale)) {
      return false;
    }
    Scale o = (Scale) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}