// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class LocalVariableDeclaration implements Serializable, Comparable<LocalVariableDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.LocalVariableDeclaration");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name DECLARATORS = new hydra.core.Name("declarators");

  public final java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers;

  public final hydra.ext.java.syntax.LocalVariableType type;

  public final java.util.List<hydra.ext.java.syntax.VariableDeclarator> declarators;

  public LocalVariableDeclaration (java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers, hydra.ext.java.syntax.LocalVariableType type, java.util.List<hydra.ext.java.syntax.VariableDeclarator> declarators) {
    this.modifiers = modifiers;
    this.type = type;
    this.declarators = declarators;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LocalVariableDeclaration)) {
      return false;
    }
    LocalVariableDeclaration o = (LocalVariableDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.declarators,
      o.declarators);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(declarators);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LocalVariableDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      modifiers,
      other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      declarators,
      other.declarators);
  }

  public LocalVariableDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.VariableModifier> modifiers) {
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }

  public LocalVariableDeclaration withType(hydra.ext.java.syntax.LocalVariableType type) {
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }

  public LocalVariableDeclaration withDeclarators(java.util.List<hydra.ext.java.syntax.VariableDeclarator> declarators) {
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }
}
