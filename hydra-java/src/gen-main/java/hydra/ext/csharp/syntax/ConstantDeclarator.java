// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ConstantDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ConstantDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.ext.csharp.syntax.ConstantExpression expression;
  
  public ConstantDeclarator (hydra.ext.csharp.syntax.Identifier identifier, hydra.ext.csharp.syntax.ConstantExpression expression) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((expression));
    this.identifier = identifier;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantDeclarator)) {
      return false;
    }
    ConstantDeclarator o = (ConstantDeclarator) (other);
    return identifier.equals(o.identifier) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * expression.hashCode();
  }
  
  public ConstantDeclarator withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ConstantDeclarator(identifier, expression);
  }
  
  public ConstantDeclarator withExpression(hydra.ext.csharp.syntax.ConstantExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ConstantDeclarator(identifier, expression);
  }
}