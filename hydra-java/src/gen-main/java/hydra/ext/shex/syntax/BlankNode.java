package hydra.ext.shex.syntax;

public class BlankNode {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.BlankNode");
  
  public final hydra.ext.shex.syntax.BlankNodeLabel value;
  
  public BlankNode (hydra.ext.shex.syntax.BlankNodeLabel value) {
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