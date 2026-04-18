// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An if statement
 */
public class IfStatement implements Serializable, Comparable<IfStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.IfStatement");

  public static final hydra.core.Name TEST = new hydra.core.Name("test");

  public static final hydra.core.Name CONSEQUENT = new hydra.core.Name("consequent");

  public static final hydra.core.Name ALTERNATE = new hydra.core.Name("alternate");

  public final hydra.javaScript.syntax.Expression test;

  public final hydra.javaScript.syntax.Statement consequent;

  public final hydra.util.Maybe<hydra.javaScript.syntax.Statement> alternate;

  public IfStatement (hydra.javaScript.syntax.Expression test, hydra.javaScript.syntax.Statement consequent, hydra.util.Maybe<hydra.javaScript.syntax.Statement> alternate) {
    this.test = test;
    this.consequent = consequent;
    this.alternate = alternate;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfStatement)) {
      return false;
    }
    IfStatement o = (IfStatement) other;
    return java.util.Objects.equals(
      this.test,
      o.test) && java.util.Objects.equals(
      this.consequent,
      o.consequent) && java.util.Objects.equals(
      this.alternate,
      o.alternate);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(test) + 3 * java.util.Objects.hashCode(consequent) + 5 * java.util.Objects.hashCode(alternate);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IfStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      test,
      other.test);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      consequent,
      other.consequent);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      alternate,
      other.alternate);
  }

  public IfStatement withTest(hydra.javaScript.syntax.Expression test) {
    return new IfStatement(test, consequent, alternate);
  }

  public IfStatement withConsequent(hydra.javaScript.syntax.Statement consequent) {
    return new IfStatement(test, consequent, alternate);
  }

  public IfStatement withAlternate(hydra.util.Maybe<hydra.javaScript.syntax.Statement> alternate) {
    return new IfStatement(test, consequent, alternate);
  }
}
