// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class BlankNode implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.BlankNode");
  
  public final hydra.langs.shex.syntax.BlankNodeLabel value;
  
  public BlankNode (hydra.langs.shex.syntax.BlankNodeLabel value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
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