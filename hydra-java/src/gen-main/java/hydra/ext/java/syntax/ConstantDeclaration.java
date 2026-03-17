// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ConstantDeclaration implements Serializable, Comparable<ConstantDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.ConstantDeclaration");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name VARIABLES = new hydra.core.Name("variables");

  public final hydra.util.ConsList<hydra.ext.java.syntax.ConstantModifier> modifiers;

  public final hydra.ext.java.syntax.UnannType type;

  public final hydra.util.ConsList<hydra.ext.java.syntax.VariableDeclarator> variables;

  public ConstantDeclaration (hydra.util.ConsList<hydra.ext.java.syntax.ConstantModifier> modifiers, hydra.ext.java.syntax.UnannType type, hydra.util.ConsList<hydra.ext.java.syntax.VariableDeclarator> variables) {
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
    cmp = ((Comparable) modifiers).compareTo(other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) variables).compareTo(other.variables);
  }

  public ConstantDeclaration withModifiers(hydra.util.ConsList<hydra.ext.java.syntax.ConstantModifier> modifiers) {
    return new ConstantDeclaration(modifiers, type, variables);
  }

  public ConstantDeclaration withType(hydra.ext.java.syntax.UnannType type) {
    return new ConstantDeclaration(modifiers, type, variables);
  }

  public ConstantDeclaration withVariables(hydra.util.ConsList<hydra.ext.java.syntax.VariableDeclarator> variables) {
    return new ConstantDeclaration(modifiers, type, variables);
  }
}
