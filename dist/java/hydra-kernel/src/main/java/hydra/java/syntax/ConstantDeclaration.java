// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ConstantDeclaration implements Serializable, Comparable<ConstantDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ConstantDeclaration");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name VARIABLES = new hydra.core.Name("variables");

  public final java.util.List<hydra.java.syntax.ConstantModifier> modifiers;

  public final hydra.java.syntax.UnannType type;

  public final java.util.List<hydra.java.syntax.VariableDeclarator> variables;

  public ConstantDeclaration (java.util.List<hydra.java.syntax.ConstantModifier> modifiers, hydra.java.syntax.UnannType type, java.util.List<hydra.java.syntax.VariableDeclarator> variables) {
    this.modifiers = modifiers;
    this.type = type;
    this.variables = variables;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantDeclaration)) {
      return false;
    }
    ConstantDeclaration o = (ConstantDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.variables,
      o.variables);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(variables);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstantDeclaration other) {
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
      variables,
      other.variables);
  }

  public ConstantDeclaration withModifiers(java.util.List<hydra.java.syntax.ConstantModifier> modifiers) {
    return new ConstantDeclaration(modifiers, type, variables);
  }

  public ConstantDeclaration withType(hydra.java.syntax.UnannType type) {
    return new ConstantDeclaration(modifiers, type, variables);
  }

  public ConstantDeclaration withVariables(java.util.List<hydra.java.syntax.VariableDeclarator> variables) {
    return new ConstantDeclaration(modifiers, type, variables);
  }
}
