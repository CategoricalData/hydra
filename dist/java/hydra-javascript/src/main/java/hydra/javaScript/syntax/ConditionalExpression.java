// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A conditional (ternary) expression: test ? consequent : alternate
 */
public class ConditionalExpression implements Serializable, Comparable<ConditionalExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ConditionalExpression");

  public static final hydra.core.Name TEST = new hydra.core.Name("test");

  public static final hydra.core.Name CONSEQUENT = new hydra.core.Name("consequent");

  public static final hydra.core.Name ALTERNATE = new hydra.core.Name("alternate");

  public final hydra.javaScript.syntax.Expression test;

  public final hydra.javaScript.syntax.Expression consequent;

  public final hydra.javaScript.syntax.Expression alternate;

  public ConditionalExpression (hydra.javaScript.syntax.Expression test, hydra.javaScript.syntax.Expression consequent, hydra.javaScript.syntax.Expression alternate) {
    this.test = test;
    this.consequent = consequent;
    this.alternate = alternate;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConditionalExpression)) {
      return false;
    }
    ConditionalExpression o = (ConditionalExpression) other;
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
  public int compareTo(ConditionalExpression other) {
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

  public ConditionalExpression withTest(hydra.javaScript.syntax.Expression test) {
    return new ConditionalExpression(test, consequent, alternate);
  }

  public ConditionalExpression withConsequent(hydra.javaScript.syntax.Expression consequent) {
    return new ConditionalExpression(test, consequent, alternate);
  }

  public ConditionalExpression withAlternate(hydra.javaScript.syntax.Expression alternate) {
    return new ConditionalExpression(test, consequent, alternate);
  }
}
