package hydra.ext.shex.syntax;

public class Exclusion {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Exclusion");
  
  public final hydra.ext.shex.syntax.Iri value;
  
  public Exclusion (hydra.ext.shex.syntax.Iri value) {
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