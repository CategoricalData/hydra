package hydra.ext.sql.ansi;

public class ReferenceType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ReferenceType");
  
  public ReferenceType () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReferenceType)) {
      return false;
    }
    ReferenceType o = (ReferenceType) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}