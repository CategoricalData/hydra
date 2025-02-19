// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ImplicitlyTypedLocalVariableDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ImplicitlyTypedLocalVariableDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public ImplicitlyTypedLocalVariableDeclarator (hydra.ext.csharp.syntax.Identifier identifier, hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((expression));
    this.identifier = identifier;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImplicitlyTypedLocalVariableDeclarator)) {
      return false;
    }
    ImplicitlyTypedLocalVariableDeclarator o = (ImplicitlyTypedLocalVariableDeclarator) (other);
    return identifier.equals(o.identifier) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * expression.hashCode();
  }
  
  public ImplicitlyTypedLocalVariableDeclarator withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ImplicitlyTypedLocalVariableDeclarator(identifier, expression);
  }
  
  public ImplicitlyTypedLocalVariableDeclarator withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ImplicitlyTypedLocalVariableDeclarator(identifier, expression);
  }
}