// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class ListComprehension implements Serializable, Comparable<ListComprehension> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.ListComprehension");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.cypher.openCypher.FilterExpression left;

  public final hydra.util.Maybe<hydra.cypher.openCypher.Expression> right;

  public ListComprehension (hydra.cypher.openCypher.FilterExpression left, hydra.util.Maybe<hydra.cypher.openCypher.Expression> right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListComprehension)) {
      return false;
    }
    ListComprehension o = (ListComprehension) other;
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
  public int compareTo(ListComprehension other) {
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

  public ListComprehension withLeft(hydra.cypher.openCypher.FilterExpression left) {
    return new ListComprehension(left, right);
  }

  public ListComprehension withRight(hydra.util.Maybe<hydra.cypher.openCypher.Expression> right) {
    return new ListComprehension(left, right);
  }
}
