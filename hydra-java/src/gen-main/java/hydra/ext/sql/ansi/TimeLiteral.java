package hydra.ext.sql.ansi;

public class TimeLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.TimeLiteral");
  
  public final hydra.ext.sql.ansi.TimeString value;
  
  public TimeLiteral (hydra.ext.sql.ansi.TimeString value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TimeLiteral)) {
      return false;
    }
    TimeLiteral o = (TimeLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}