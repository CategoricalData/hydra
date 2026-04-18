// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class BlankNode implements Serializable, Comparable<BlankNode> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.BlankNode");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.shex.syntax.BlankNodeLabel value;

  public BlankNode (hydra.shex.syntax.BlankNodeLabel value) {
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
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
