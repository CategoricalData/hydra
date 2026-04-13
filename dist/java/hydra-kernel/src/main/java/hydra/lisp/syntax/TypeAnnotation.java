// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * An expression with a type annotation
 */
public class TypeAnnotation implements Serializable, Comparable<TypeAnnotation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.TypeAnnotation");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The annotated expression
   */
  public final hydra.lisp.syntax.Expression expression;

  /**
   * The type specifier
   */
  public final hydra.lisp.syntax.TypeSpecifier type;

  public TypeAnnotation (hydra.lisp.syntax.Expression expression, hydra.lisp.syntax.TypeSpecifier type) {
    this.expression = expression;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeAnnotation)) {
      return false;
    }
    TypeAnnotation o = (TypeAnnotation) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeAnnotation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public TypeAnnotation withExpression(hydra.lisp.syntax.Expression expression) {
    return new TypeAnnotation(expression, type);
  }

  public TypeAnnotation withType(hydra.lisp.syntax.TypeSpecifier type) {
    return new TypeAnnotation(expression, type);
  }
}
