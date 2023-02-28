package hydra.langs.shex.syntax;

public class Exclusion {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.Exclusion");
  
  public final hydra.langs.shex.syntax.Iri value;
  
  public Exclusion (hydra.langs.shex.syntax.Iri value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Exclusion)) {
      return false;
    }
    Exclusion o = (Exclusion) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}