// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class EqualityPair implements Serializable, Comparable<EqualityPair> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.EqualityPair");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.graphviz.dot.Id left;

  public final hydra.graphviz.dot.Id right;

  public EqualityPair (hydra.graphviz.dot.Id left, hydra.graphviz.dot.Id right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EqualityPair)) {
      return false;
    }
    EqualityPair o = (EqualityPair) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EqualityPair other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      left,
      other.left);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      right,
      other.right);
  }

  public EqualityPair withLeft(hydra.graphviz.dot.Id left) {
    return new EqualityPair(left, right);
  }

  public EqualityPair withRight(hydra.graphviz.dot.Id right) {
    return new EqualityPair(left, right);
  }
}
