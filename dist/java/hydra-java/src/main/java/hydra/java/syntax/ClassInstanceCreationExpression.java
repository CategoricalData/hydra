// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ClassInstanceCreationExpression implements Serializable, Comparable<ClassInstanceCreationExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ClassInstanceCreationExpression");

  public static final hydra.core.Name QUALIFIER = new hydra.core.Name("qualifier");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.util.Maybe<hydra.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier;

  public final hydra.java.syntax.UnqualifiedClassInstanceCreationExpression expression;

  public ClassInstanceCreationExpression (hydra.util.Maybe<hydra.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier, hydra.java.syntax.UnqualifiedClassInstanceCreationExpression expression) {
    this.qualifier = qualifier;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassInstanceCreationExpression)) {
      return false;
    }
    ClassInstanceCreationExpression o = (ClassInstanceCreationExpression) other;
    return java.util.Objects.equals(
      this.qualifier,
      o.qualifier) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualifier) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClassInstanceCreationExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      qualifier,
      other.qualifier);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public ClassInstanceCreationExpression withQualifier(hydra.util.Maybe<hydra.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier) {
    return new ClassInstanceCreationExpression(qualifier, expression);
  }

  public ClassInstanceCreationExpression withExpression(hydra.java.syntax.UnqualifiedClassInstanceCreationExpression expression) {
    return new ClassInstanceCreationExpression(qualifier, expression);
  }
}
