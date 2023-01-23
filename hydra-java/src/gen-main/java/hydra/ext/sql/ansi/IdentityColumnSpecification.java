package hydra.ext.sql.ansi;

public class IdentityColumnSpecification {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.IdentityColumnSpecification");
  
  public IdentityColumnSpecification () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IdentityColumnSpecification)) {
      return false;
    }
    IdentityColumnSpecification o = (IdentityColumnSpecification) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}