package hydra.ext.sql.ansi;

public class GeneralValueSpecification {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.GeneralValueSpecification");
  
  public GeneralValueSpecification () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GeneralValueSpecification)) {
      return false;
    }
    GeneralValueSpecification o = (GeneralValueSpecification) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}