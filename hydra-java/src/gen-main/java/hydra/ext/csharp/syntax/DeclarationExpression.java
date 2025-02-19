// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class DeclarationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DeclarationExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.csharp.syntax.LocalVariableType type;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public DeclarationExpression (hydra.ext.csharp.syntax.LocalVariableType type, hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    this.type = type;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeclarationExpression)) {
      return false;
    }
    DeclarationExpression o = (DeclarationExpression) (other);
    return type.equals(o.type) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * identifier.hashCode();
  }
  
  public DeclarationExpression withType(hydra.ext.csharp.syntax.LocalVariableType type) {
    java.util.Objects.requireNonNull((type));
    return new DeclarationExpression(type, identifier);
  }
  
  public DeclarationExpression withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new DeclarationExpression(type, identifier);
  }
}