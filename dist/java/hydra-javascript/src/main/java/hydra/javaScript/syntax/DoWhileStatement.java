// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A do-while statement
 */
public class DoWhileStatement implements Serializable, Comparable<DoWhileStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.DoWhileStatement");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name TEST = new hydra.core.Name("test");

  public final hydra.javaScript.syntax.Statement body;

  public final hydra.javaScript.syntax.Expression test;

  public DoWhileStatement (hydra.javaScript.syntax.Statement body, hydra.javaScript.syntax.Expression test) {
    this.body = body;
    this.test = test;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DoWhileStatement)) {
      return false;
    }
    DoWhileStatement o = (DoWhileStatement) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.test,
      o.test);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(test);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DoWhileStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      test,
      other.test);
  }

  public DoWhileStatement withBody(hydra.javaScript.syntax.Statement body) {
    return new DoWhileStatement(body, test);
  }

  public DoWhileStatement withTest(hydra.javaScript.syntax.Expression test) {
    return new DoWhileStatement(body, test);
  }
}
