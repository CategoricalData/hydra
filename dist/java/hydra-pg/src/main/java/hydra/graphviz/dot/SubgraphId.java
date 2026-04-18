// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class SubgraphId implements Serializable, Comparable<SubgraphId> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.SubgraphId");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.Maybe<hydra.graphviz.dot.Id> value;

  public SubgraphId (hydra.util.Maybe<hydra.graphviz.dot.Id> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubgraphId)) {
      return false;
    }
    SubgraphId o = (SubgraphId) other;
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
  public int compareTo(SubgraphId other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
