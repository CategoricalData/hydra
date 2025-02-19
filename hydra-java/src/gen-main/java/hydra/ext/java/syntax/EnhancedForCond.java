// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnhancedForCond implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.EnhancedForCond");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;
  
  public final hydra.ext.java.syntax.LocalVariableType type;
  
  public final hydra.ext.java.syntax.VariableDeclaratorId id;
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public EnhancedForCond (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.LocalVariableType type, hydra.ext.java.syntax.VariableDeclaratorId id, hydra.ext.java.syntax.Expression expression) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((expression));
    this.modifiers = modifiers;
    this.type = type;
    this.id = id;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnhancedForCond)) {
      return false;
    }
    EnhancedForCond o = (EnhancedForCond) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && id.equals(o.id) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * id.hashCode() + 7 * expression.hashCode();
  }
  
  public EnhancedForCond withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new EnhancedForCond(modifiers, type, id, expression);
  }
  
  public EnhancedForCond withType(hydra.ext.java.syntax.LocalVariableType type) {
    java.util.Objects.requireNonNull((type));
    return new EnhancedForCond(modifiers, type, id, expression);
  }
  
  public EnhancedForCond withId(hydra.ext.java.syntax.VariableDeclaratorId id) {
    java.util.Objects.requireNonNull((id));
    return new EnhancedForCond(modifiers, type, id, expression);
  }
  
  public EnhancedForCond withExpression(hydra.ext.java.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new EnhancedForCond(modifiers, type, id, expression);
  }
}