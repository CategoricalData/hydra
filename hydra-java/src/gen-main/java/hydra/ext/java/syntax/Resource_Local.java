// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class Resource_Local implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.Resource_Local");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;
  
  public final hydra.ext.java.syntax.LocalVariableType type;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public Resource_Local (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.LocalVariableType type, hydra.ext.java.syntax.Identifier identifier, hydra.ext.java.syntax.Expression expression) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((expression));
    this.modifiers = modifiers;
    this.type = type;
    this.identifier = identifier;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Resource_Local)) {
      return false;
    }
    Resource_Local o = (Resource_Local) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && identifier.equals(o.identifier) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * identifier.hashCode() + 7 * expression.hashCode();
  }
  
  public Resource_Local withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new Resource_Local(modifiers, type, identifier, expression);
  }
  
  public Resource_Local withType(hydra.ext.java.syntax.LocalVariableType type) {
    java.util.Objects.requireNonNull((type));
    return new Resource_Local(modifiers, type, identifier, expression);
  }
  
  public Resource_Local withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new Resource_Local(modifiers, type, identifier, expression);
  }
  
  public Resource_Local withExpression(hydra.ext.java.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new Resource_Local(modifiers, type, identifier, expression);
  }
}