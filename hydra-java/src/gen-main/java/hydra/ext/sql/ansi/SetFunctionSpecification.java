package hydra.ext.sql.ansi;

public class SetFunctionSpecification {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.SetFunctionSpecification");
  
  public SetFunctionSpecification () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SetFunctionSpecification)) {
      return false;
    }
    SetFunctionSpecification o = (SetFunctionSpecification) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}