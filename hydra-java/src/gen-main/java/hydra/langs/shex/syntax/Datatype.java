package hydra.langs.shex.syntax;

public class Datatype {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.Datatype");
  
  public final hydra.langs.shex.syntax.Iri value;
  
  public Datatype (hydra.langs.shex.syntax.Iri value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Datatype)) {
      return false;
    }
    Datatype o = (Datatype) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}