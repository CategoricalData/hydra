// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ConstantDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.ConstantDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLES = new hydra.core.Name("variables");
  
  public final java.util.List<hydra.langs.java.syntax.ConstantModifier> modifiers;
  
  public final hydra.langs.java.syntax.UnannType type;
  
  public final java.util.List<hydra.langs.java.syntax.VariableDeclarator> variables;
  
  public ConstantDeclaration (java.util.List<hydra.langs.java.syntax.ConstantModifier> modifiers, hydra.langs.java.syntax.UnannType type, java.util.List<hydra.langs.java.syntax.VariableDeclarator> variables) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((variables));
    this.modifiers = modifiers;
    this.type = type;
    this.variables = variables;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantDeclaration)) {
      return false;
    }
    ConstantDeclaration o = (ConstantDeclaration) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && variables.equals(o.variables);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * variables.hashCode();
  }
  
  public ConstantDeclaration withModifiers(java.util.List<hydra.langs.java.syntax.ConstantModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new ConstantDeclaration(modifiers, type, variables);
  }
  
  public ConstantDeclaration withType(hydra.langs.java.syntax.UnannType type) {
    java.util.Objects.requireNonNull((type));
    return new ConstantDeclaration(modifiers, type, variables);
  }
  
  public ConstantDeclaration withVariables(java.util.List<hydra.langs.java.syntax.VariableDeclarator> variables) {
    java.util.Objects.requireNonNull((variables));
    return new ConstantDeclaration(modifiers, type, variables);
  }
}