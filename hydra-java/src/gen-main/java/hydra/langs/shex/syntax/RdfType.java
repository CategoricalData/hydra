package hydra.langs.shex.syntax;

public class RdfType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.RdfType");
  
  public RdfType () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RdfType)) {
      return false;
    }
    RdfType o = (RdfType) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}