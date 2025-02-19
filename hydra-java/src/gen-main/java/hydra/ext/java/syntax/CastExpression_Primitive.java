// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CastExpression_Primitive implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CastExpression_Primitive");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type;
  
  public final hydra.ext.java.syntax.UnaryExpression expression;
  
  public CastExpression_Primitive (hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type, hydra.ext.java.syntax.UnaryExpression expression) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((expression));
    this.type = type;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_Primitive)) {
      return false;
    }
    CastExpression_Primitive o = (CastExpression_Primitive) (other);
    return type.equals(o.type) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * expression.hashCode();
  }
  
  public CastExpression_Primitive withType(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type) {
    java.util.Objects.requireNonNull((type));
    return new CastExpression_Primitive(type, expression);
  }
  
  public CastExpression_Primitive withExpression(hydra.ext.java.syntax.UnaryExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new CastExpression_Primitive(type, expression);
  }
}