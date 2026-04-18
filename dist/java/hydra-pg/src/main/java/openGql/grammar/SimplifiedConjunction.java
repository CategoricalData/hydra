// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SimplifiedConjunction implements Serializable, Comparable<SimplifiedConjunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedConjunction");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final openGql.grammar.SimplifiedFactorLow left;

  public final openGql.grammar.SimplifiedFactorHigh right;

  public SimplifiedConjunction (openGql.grammar.SimplifiedFactorLow left, openGql.grammar.SimplifiedFactorHigh right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimplifiedConjunction)) {
      return false;
    }
    SimplifiedConjunction o = (SimplifiedConjunction) other;
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
  public int compareTo(SimplifiedConjunction other) {
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

  public SimplifiedConjunction withLeft(openGql.grammar.SimplifiedFactorLow left) {
    return new SimplifiedConjunction(left, right);
  }

  public SimplifiedConjunction withRight(openGql.grammar.SimplifiedFactorHigh right) {
    return new SimplifiedConjunction(left, right);
  }
}
