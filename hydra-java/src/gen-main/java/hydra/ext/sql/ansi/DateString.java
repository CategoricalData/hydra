package hydra.ext.sql.ansi;

public class DateString {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.DateString");
  
  public DateString () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DateString)) {
      return false;
    }
    DateString o = (DateString) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}