package hydra.langs.owl.syntax;

public class DataProperty {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.DataProperty");
  
  public DataProperty () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataProperty)) {
      return false;
    }
    DataProperty o = (DataProperty) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}