package hydra.ext.rdf.syntax;

public class BlankNode {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/rdf/syntax.BlankNode");
  
  public final String value;
  
  public BlankNode (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BlankNode)) {
      return false;
    }
    BlankNode o = (BlankNode) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}