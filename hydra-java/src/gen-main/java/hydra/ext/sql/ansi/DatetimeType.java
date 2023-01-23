package hydra.ext.sql.ansi;

public class DatetimeType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.DatetimeType");
  
  public DatetimeType () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatetimeType)) {
      return false;
    }
    DatetimeType o = (DatetimeType) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}