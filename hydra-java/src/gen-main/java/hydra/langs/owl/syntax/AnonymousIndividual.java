package hydra.langs.owl.syntax;

public class AnonymousIndividual {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.AnonymousIndividual");
  
  public AnonymousIndividual () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnonymousIndividual)) {
      return false;
    }
    AnonymousIndividual o = (AnonymousIndividual) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}