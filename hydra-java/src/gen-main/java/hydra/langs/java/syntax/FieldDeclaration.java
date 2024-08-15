// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class FieldDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.FieldDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_UNANN_TYPE = new hydra.core.Name("unannType");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE_DECLARATORS = new hydra.core.Name("variableDeclarators");
  
  public final java.util.List<hydra.langs.java.syntax.FieldModifier> modifiers;
  
  public final hydra.langs.java.syntax.UnannType unannType;
  
  public final java.util.List<hydra.langs.java.syntax.VariableDeclarator> variableDeclarators;
  
  public FieldDeclaration (java.util.List<hydra.langs.java.syntax.FieldModifier> modifiers, hydra.langs.java.syntax.UnannType unannType, java.util.List<hydra.langs.java.syntax.VariableDeclarator> variableDeclarators) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((unannType));
    java.util.Objects.requireNonNull((variableDeclarators));
    this.modifiers = modifiers;
    this.unannType = unannType;
    this.variableDeclarators = variableDeclarators;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldDeclaration)) {
      return false;
    }
    FieldDeclaration o = (FieldDeclaration) (other);
    return modifiers.equals(o.modifiers) && unannType.equals(o.unannType) && variableDeclarators.equals(o.variableDeclarators);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * unannType.hashCode() + 5 * variableDeclarators.hashCode();
  }
  
  public FieldDeclaration withModifiers(java.util.List<hydra.langs.java.syntax.FieldModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
  
  public FieldDeclaration withUnannType(hydra.langs.java.syntax.UnannType unannType) {
    java.util.Objects.requireNonNull((unannType));
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
  
  public FieldDeclaration withVariableDeclarators(java.util.List<hydra.langs.java.syntax.VariableDeclarator> variableDeclarators) {
    java.util.Objects.requireNonNull((variableDeclarators));
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
}