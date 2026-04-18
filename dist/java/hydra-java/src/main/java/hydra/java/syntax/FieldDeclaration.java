// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class FieldDeclaration implements Serializable, Comparable<FieldDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.FieldDeclaration");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name UNANN_TYPE = new hydra.core.Name("unannType");

  public static final hydra.core.Name VARIABLE_DECLARATORS = new hydra.core.Name("variableDeclarators");

  public final java.util.List<hydra.java.syntax.FieldModifier> modifiers;

  public final hydra.java.syntax.UnannType unannType;

  public final java.util.List<hydra.java.syntax.VariableDeclarator> variableDeclarators;

  public FieldDeclaration (java.util.List<hydra.java.syntax.FieldModifier> modifiers, hydra.java.syntax.UnannType unannType, java.util.List<hydra.java.syntax.VariableDeclarator> variableDeclarators) {
    this.modifiers = modifiers;
    this.unannType = unannType;
    this.variableDeclarators = variableDeclarators;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldDeclaration)) {
      return false;
    }
    FieldDeclaration o = (FieldDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.unannType,
      o.unannType) && java.util.Objects.equals(
      this.variableDeclarators,
      o.variableDeclarators);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(unannType) + 5 * java.util.Objects.hashCode(variableDeclarators);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      modifiers,
      other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      unannType,
      other.unannType);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      variableDeclarators,
      other.variableDeclarators);
  }

  public FieldDeclaration withModifiers(java.util.List<hydra.java.syntax.FieldModifier> modifiers) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }

  public FieldDeclaration withUnannType(hydra.java.syntax.UnannType unannType) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }

  public FieldDeclaration withVariableDeclarators(java.util.List<hydra.java.syntax.VariableDeclarator> variableDeclarators) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
}
