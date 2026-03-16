// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class FieldDeclaration implements Serializable, Comparable<FieldDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.FieldDeclaration");
  
  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name UNANN_TYPE = new hydra.core.Name("unannType");
  
  public static final hydra.core.Name VARIABLE_DECLARATORS = new hydra.core.Name("variableDeclarators");
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.FieldModifier> modifiers;
  
  public final hydra.ext.java.syntax.UnannType unannType;
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.VariableDeclarator> variableDeclarators;
  
  public FieldDeclaration (hydra.util.ConsList<hydra.ext.java.syntax.FieldModifier> modifiers, hydra.ext.java.syntax.UnannType unannType, hydra.util.ConsList<hydra.ext.java.syntax.VariableDeclarator> variableDeclarators) {
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
    cmp = ((Comparable) modifiers).compareTo(other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) unannType).compareTo(other.unannType);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) variableDeclarators).compareTo(other.variableDeclarators);
  }
  
  public FieldDeclaration withModifiers(hydra.util.ConsList<hydra.ext.java.syntax.FieldModifier> modifiers) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
  
  public FieldDeclaration withUnannType(hydra.ext.java.syntax.UnannType unannType) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
  
  public FieldDeclaration withVariableDeclarators(hydra.util.ConsList<hydra.ext.java.syntax.VariableDeclarator> variableDeclarators) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
}
