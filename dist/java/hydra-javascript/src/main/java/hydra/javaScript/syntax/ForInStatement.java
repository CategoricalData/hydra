// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A for-in statement
 */
public class ForInStatement implements Serializable, Comparable<ForInStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ForInStatement");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.javaScript.syntax.ForInLeft left;

  public final hydra.javaScript.syntax.Expression right;

  public final hydra.javaScript.syntax.Statement body;

  public ForInStatement (hydra.javaScript.syntax.ForInLeft left, hydra.javaScript.syntax.Expression right, hydra.javaScript.syntax.Statement body) {
    this.left = left;
    this.right = right;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForInStatement)) {
      return false;
    }
    ForInStatement o = (ForInStatement) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right) + 5 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForInStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      left,
      other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      right,
      other.right);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public ForInStatement withLeft(hydra.javaScript.syntax.ForInLeft left) {
    return new ForInStatement(left, right, body);
  }

  public ForInStatement withRight(hydra.javaScript.syntax.Expression right) {
    return new ForInStatement(left, right, body);
  }

  public ForInStatement withBody(hydra.javaScript.syntax.Statement body) {
    return new ForInStatement(left, right, body);
  }
}
