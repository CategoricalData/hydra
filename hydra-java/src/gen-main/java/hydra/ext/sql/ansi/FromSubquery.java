package hydra.ext.sql.ansi;

public class FromSubquery {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.FromSubquery");
  
  public FromSubquery () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FromSubquery)) {
      return false;
    }
    FromSubquery o = (FromSubquery) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}