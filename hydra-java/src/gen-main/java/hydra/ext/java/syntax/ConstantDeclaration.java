package hydra.ext.java.syntax;

public class ConstantDeclaration {
  public final java.util.List<ConstantModifier> modifiers;
  
  public final UnannType type;
  
  public final java.util.List<VariableDeclarator> variables;
  
  public ConstantDeclaration (java.util.List<ConstantModifier> modifiers, UnannType type, java.util.List<VariableDeclarator> variables) {
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
  
  public ConstantDeclaration withModifiers(java.util.List<ConstantModifier> modifiers) {
    return new ConstantDeclaration(modifiers, type, variables);
  }
  
  public ConstantDeclaration withType(UnannType type) {
    return new ConstantDeclaration(modifiers, type, variables);
  }
  
  public ConstantDeclaration withVariables(java.util.List<VariableDeclarator> variables) {
    return new ConstantDeclaration(modifiers, type, variables);
  }
}