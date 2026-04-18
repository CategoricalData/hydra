// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A for-of statement
 */
public class ForOfStatement implements Serializable, Comparable<ForOfStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ForOfStatement");

  public static final hydra.core.Name AWAIT = new hydra.core.Name("await");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * Whether this is a for-await-of
   */
  public final Boolean await;

  public final hydra.javaScript.syntax.ForInLeft left;

  public final hydra.javaScript.syntax.Expression right;

  public final hydra.javaScript.syntax.Statement body;

  public ForOfStatement (Boolean await, hydra.javaScript.syntax.ForInLeft left, hydra.javaScript.syntax.Expression right, hydra.javaScript.syntax.Statement body) {
    this.await = await;
    this.left = left;
    this.right = right;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForOfStatement)) {
      return false;
    }
    ForOfStatement o = (ForOfStatement) other;
    return java.util.Objects.equals(
      this.await,
      o.await) && java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(await) + 3 * java.util.Objects.hashCode(left) + 5 * java.util.Objects.hashCode(right) + 7 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForOfStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      await,
      other.await);
    if (cmp != 0) {
      return cmp;
    }
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

  public ForOfStatement withAwait(Boolean await) {
    return new ForOfStatement(await, left, right, body);
  }

  public ForOfStatement withLeft(hydra.javaScript.syntax.ForInLeft left) {
    return new ForOfStatement(await, left, right, body);
  }

  public ForOfStatement withRight(hydra.javaScript.syntax.Expression right) {
    return new ForOfStatement(await, left, right, body);
  }

  public ForOfStatement withBody(hydra.javaScript.syntax.Statement body) {
    return new ForOfStatement(await, left, right, body);
  }
}
