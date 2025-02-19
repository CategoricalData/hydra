// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class UnaryOperatorDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.UnaryOperatorDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.OverloadableUnaryOperator operator;
  
  public final hydra.ext.csharp.syntax.FixedParameter parameter;
  
  public UnaryOperatorDeclarator (hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.OverloadableUnaryOperator operator, hydra.ext.csharp.syntax.FixedParameter parameter) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((parameter));
    this.type = type;
    this.operator = operator;
    this.parameter = parameter;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryOperatorDeclarator)) {
      return false;
    }
    UnaryOperatorDeclarator o = (UnaryOperatorDeclarator) (other);
    return type.equals(o.type) && operator.equals(o.operator) && parameter.equals(o.parameter);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * operator.hashCode() + 5 * parameter.hashCode();
  }
  
  public UnaryOperatorDeclarator withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new UnaryOperatorDeclarator(type, operator, parameter);
  }
  
  public UnaryOperatorDeclarator withOperator(hydra.ext.csharp.syntax.OverloadableUnaryOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new UnaryOperatorDeclarator(type, operator, parameter);
  }
  
  public UnaryOperatorDeclarator withParameter(hydra.ext.csharp.syntax.FixedParameter parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new UnaryOperatorDeclarator(type, operator, parameter);
  }
}