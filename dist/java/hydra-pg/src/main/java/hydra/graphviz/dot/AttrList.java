// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class AttrList implements Serializable, Comparable<AttrList> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.AttrList");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<java.util.List<hydra.graphviz.dot.EqualityPair>> value;

  public AttrList (java.util.List<java.util.List<hydra.graphviz.dot.EqualityPair>> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AttrList)) {
      return false;
    }
    AttrList o = (AttrList) other;
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
  public int compareTo(AttrList other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
