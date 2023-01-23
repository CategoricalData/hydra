package hydra.ext.sql.ansi;

public class Length {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.Length");
  
  public final hydra.ext.sql.ansi.UnsignedInteger value;
  
  public Length (hydra.ext.sql.ansi.UnsignedInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Length)) {
      return false;
    }
    Length o = (Length) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}