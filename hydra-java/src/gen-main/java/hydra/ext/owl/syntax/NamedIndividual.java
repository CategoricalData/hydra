package hydra.ext.owl.syntax;

public class NamedIndividual {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.NamedIndividual");
  
  public NamedIndividual () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedIndividual)) {
      return false;
    }
    NamedIndividual o = (NamedIndividual) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}