// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NullConditionalProjectionInitializer implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NullConditionalProjectionInitializer");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final hydra.ext.csharp.syntax.PrimaryExpression expression;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments;
  
  public NullConditionalProjectionInitializer (hydra.ext.csharp.syntax.PrimaryExpression expression, hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((typeArguments));
    this.expression = expression;
    this.identifier = identifier;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullConditionalProjectionInitializer)) {
      return false;
    }
    NullConditionalProjectionInitializer o = (NullConditionalProjectionInitializer) (other);
    return expression.equals(o.expression) && identifier.equals(o.identifier) && typeArguments.equals(o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * identifier.hashCode() + 5 * typeArguments.hashCode();
  }
  
  public NullConditionalProjectionInitializer withExpression(hydra.ext.csharp.syntax.PrimaryExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new NullConditionalProjectionInitializer(expression, identifier, typeArguments);
  }
  
  public NullConditionalProjectionInitializer withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new NullConditionalProjectionInitializer(expression, identifier, typeArguments);
  }
  
  public NullConditionalProjectionInitializer withTypeArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new NullConditionalProjectionInitializer(expression, identifier, typeArguments);
  }
}