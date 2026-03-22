// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class BlankNode implements Serializable, Comparable<BlankNode> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.BlankNode");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.io.shex.syntax.BlankNodeLabel value;

  public BlankNode (hydra.ext.io.shex.syntax.BlankNodeLabel value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BlankNode)) {
      return false;
    }
    BlankNode o = (BlankNode) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BlankNode other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
