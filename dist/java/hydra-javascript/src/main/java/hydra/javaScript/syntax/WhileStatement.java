// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A while statement
 */
public class WhileStatement implements Serializable, Comparable<WhileStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.WhileStatement");

  public static final hydra.core.Name TEST = new hydra.core.Name("test");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.javaScript.syntax.Expression test;

  public final hydra.javaScript.syntax.Statement body;

  public WhileStatement (hydra.javaScript.syntax.Expression test, hydra.javaScript.syntax.Statement body) {
    this.test = test;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhileStatement)) {
      return false;
    }
    WhileStatement o = (WhileStatement) other;
    return java.util.Objects.equals(
      this.test,
      o.test) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(test) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WhileStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      test,
      other.test);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public WhileStatement withTest(hydra.javaScript.syntax.Expression test) {
    return new WhileStatement(test, body);
  }

  public WhileStatement withBody(hydra.javaScript.syntax.Statement body) {
    return new WhileStatement(test, body);
  }
}
