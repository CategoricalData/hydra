package hydra.ext.java.syntax;

public class CastExpression_Primitive {
  public final PrimitiveTypeWithAnnotations type;
  
  public final UnaryExpression expression;
  
  public CastExpression_Primitive (PrimitiveTypeWithAnnotations type, UnaryExpression expression) {
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
  
  public CastExpression_Primitive withType(PrimitiveTypeWithAnnotations type) {
    return new CastExpression_Primitive(type, expression);
  }
  
  public CastExpression_Primitive withExpression(UnaryExpression expression) {
    return new CastExpression_Primitive(type, expression);
  }
}