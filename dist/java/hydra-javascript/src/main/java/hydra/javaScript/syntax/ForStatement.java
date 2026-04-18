// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A for statement
 */
public class ForStatement implements Serializable, Comparable<ForStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ForStatement");

  public static final hydra.core.Name INIT = new hydra.core.Name("init");

  public static final hydra.core.Name TEST = new hydra.core.Name("test");

  public static final hydra.core.Name UPDATE = new hydra.core.Name("update");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * Initialization
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.ForInit> init;

  /**
   * Test condition
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.Expression> test;

  /**
   * Update expression
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.Expression> update;

  public final hydra.javaScript.syntax.Statement body;

  public ForStatement (hydra.util.Maybe<hydra.javaScript.syntax.ForInit> init, hydra.util.Maybe<hydra.javaScript.syntax.Expression> test, hydra.util.Maybe<hydra.javaScript.syntax.Expression> update, hydra.javaScript.syntax.Statement body) {
    this.init = init;
    this.test = test;
    this.update = update;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForStatement)) {
      return false;
    }
    ForStatement o = (ForStatement) other;
    return java.util.Objects.equals(
      this.init,
      o.init) && java.util.Objects.equals(
      this.test,
      o.test) && java.util.Objects.equals(
      this.update,
      o.update) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(init) + 3 * java.util.Objects.hashCode(test) + 5 * java.util.Objects.hashCode(update) + 7 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      init,
      other.init);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      test,
      other.test);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      update,
      other.update);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public ForStatement withInit(hydra.util.Maybe<hydra.javaScript.syntax.ForInit> init) {
    return new ForStatement(init, test, update, body);
  }

  public ForStatement withTest(hydra.util.Maybe<hydra.javaScript.syntax.Expression> test) {
    return new ForStatement(init, test, update, body);
  }

  public ForStatement withUpdate(hydra.util.Maybe<hydra.javaScript.syntax.Expression> update) {
    return new ForStatement(init, test, update, body);
  }

  public ForStatement withBody(hydra.javaScript.syntax.Statement body) {
    return new ForStatement(init, test, update, body);
  }
}
