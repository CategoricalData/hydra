// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class CastExpression_Primitive implements Serializable, Comparable<CastExpression_Primitive> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.CastExpression_Primitive");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.java.syntax.PrimitiveTypeWithAnnotations type;

  public final hydra.java.syntax.UnaryExpression expression;

  public CastExpression_Primitive (hydra.java.syntax.PrimitiveTypeWithAnnotations type, hydra.java.syntax.UnaryExpression expression) {
    this.type = type;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_Primitive)) {
      return false;
    }
    CastExpression_Primitive o = (CastExpression_Primitive) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CastExpression_Primitive other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public CastExpression_Primitive withType(hydra.java.syntax.PrimitiveTypeWithAnnotations type) {
    return new CastExpression_Primitive(type, expression);
  }

  public CastExpression_Primitive withExpression(hydra.java.syntax.UnaryExpression expression) {
    return new CastExpression_Primitive(type, expression);
  }
}
