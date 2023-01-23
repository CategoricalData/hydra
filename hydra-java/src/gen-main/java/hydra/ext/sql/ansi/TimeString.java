package hydra.ext.sql.ansi;

public class TimeString {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.TimeString");
  
  public TimeString () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TimeString)) {
      return false;
    }
    TimeString o = (TimeString) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}