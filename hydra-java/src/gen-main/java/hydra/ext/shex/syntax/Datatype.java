package hydra.ext.shex.syntax;

public class Datatype {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Datatype");
  
  public final hydra.ext.shex.syntax.Iri value;
  
  public Datatype (hydra.ext.shex.syntax.Iri value) {
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