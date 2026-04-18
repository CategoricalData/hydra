// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A pattern with default value (param = default)
 */
public class AssignmentPattern implements Serializable, Comparable<AssignmentPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.AssignmentPattern");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.javaScript.syntax.Pattern left;

  public final hydra.javaScript.syntax.Expression right;

  public AssignmentPattern (hydra.javaScript.syntax.Pattern left, hydra.javaScript.syntax.Expression right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssignmentPattern)) {
      return false;
    }
    AssignmentPattern o = (AssignmentPattern) other;
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
  public int compareTo(AssignmentPattern other) {
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

  public AssignmentPattern withLeft(hydra.javaScript.syntax.Pattern left) {
    return new AssignmentPattern(left, right);
  }

  public AssignmentPattern withRight(hydra.javaScript.syntax.Expression right) {
    return new AssignmentPattern(left, right);
  }
}
