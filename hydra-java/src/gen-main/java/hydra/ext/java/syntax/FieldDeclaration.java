package hydra.ext.java.syntax;

public class FieldDeclaration {
  public final java.util.List<FieldModifier> modifiers;
  
  public final UnannType unannType;
  
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<VariableDeclarator> variableDeclarators;
  
  public FieldDeclaration (java.util.List<FieldModifier> modifiers, UnannType unannType, java.util.List<VariableDeclarator> variableDeclarators) {
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
  
  public FieldDeclaration withModifiers(java.util.List<FieldModifier> modifiers) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
  
  public FieldDeclaration withUnannType(UnannType unannType) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
  
  public FieldDeclaration withVariableDeclarators(java.util.List<VariableDeclarator> variableDeclarators) {
    return new FieldDeclaration(modifiers, unannType, variableDeclarators);
  }
}