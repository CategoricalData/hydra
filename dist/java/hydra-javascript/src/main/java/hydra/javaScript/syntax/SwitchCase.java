// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A case clause in a switch statement
 */
public class SwitchCase implements Serializable, Comparable<SwitchCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.SwitchCase");

  public static final hydra.core.Name TEST = new hydra.core.Name("test");

  public static final hydra.core.Name CONSEQUENT = new hydra.core.Name("consequent");

  /**
   * The test expression (Nothing for default)
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.Expression> test;

  /**
   * The statements to execute
   */
  public final java.util.List<hydra.javaScript.syntax.Statement> consequent;

  public SwitchCase (hydra.util.Maybe<hydra.javaScript.syntax.Expression> test, java.util.List<hydra.javaScript.syntax.Statement> consequent) {
    this.test = test;
    this.consequent = consequent;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchCase)) {
      return false;
    }
    SwitchCase o = (SwitchCase) other;
    return java.util.Objects.equals(
      this.test,
      o.test) && java.util.Objects.equals(
      this.consequent,
      o.consequent);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(test) + 3 * java.util.Objects.hashCode(consequent);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SwitchCase other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      test,
      other.test);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      consequent,
      other.consequent);
  }

  public SwitchCase withTest(hydra.util.Maybe<hydra.javaScript.syntax.Expression> test) {
    return new SwitchCase(test, consequent);
  }

  public SwitchCase withConsequent(java.util.List<hydra.javaScript.syntax.Statement> consequent) {
    return new SwitchCase(test, consequent);
  }
}
