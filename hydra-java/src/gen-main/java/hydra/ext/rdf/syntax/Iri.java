package hydra.ext.rdf.syntax;

public class Iri {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/rdf/syntax.Iri");
  
  public final String value;
  
  public Iri (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Iri)) {
      return false;
    }
    Iri o = (Iri) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}