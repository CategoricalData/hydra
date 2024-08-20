// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class LocalVariableDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.LocalVariableDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATORS = new hydra.core.Name("declarators");
  
  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;
  
  public final hydra.ext.java.syntax.LocalVariableType type;
  
  public final java.util.List<hydra.ext.java.syntax.VariableDeclarator> declarators;
  
  public LocalVariableDeclaration (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.LocalVariableType type, java.util.List<hydra.ext.java.syntax.VariableDeclarator> declarators) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((declarators));
    this.modifiers = modifiers;
    this.type = type;
    this.declarators = declarators;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LocalVariableDeclaration)) {
      return false;
    }
    LocalVariableDeclaration o = (LocalVariableDeclaration) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && declarators.equals(o.declarators);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * declarators.hashCode();
  }
  
  public LocalVariableDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }
  
  public LocalVariableDeclaration withType(hydra.ext.java.syntax.LocalVariableType type) {
    java.util.Objects.requireNonNull((type));
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }
  
  public LocalVariableDeclaration withDeclarators(java.util.List<hydra.ext.java.syntax.VariableDeclarator> declarators) {
    java.util.Objects.requireNonNull((declarators));
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }
}
