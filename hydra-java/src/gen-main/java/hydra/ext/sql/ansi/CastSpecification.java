package hydra.ext.sql.ansi;

public class CastSpecification {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.CastSpecification");
  
  public CastSpecification () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastSpecification)) {
      return false;
    }
    CastSpecification o = (CastSpecification) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}