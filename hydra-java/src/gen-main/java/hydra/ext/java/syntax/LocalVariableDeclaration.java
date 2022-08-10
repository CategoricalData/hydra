package hydra.ext.java.syntax;

public class LocalVariableDeclaration {
  public final java.util.List<VariableModifier> modifiers;
  
  public final LocalVariableType type;
  
  public final java.util.List<VariableDeclarator> declarators;
  
  public LocalVariableDeclaration (java.util.List<VariableModifier> modifiers, LocalVariableType type, java.util.List<VariableDeclarator> declarators) {
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
  
  public LocalVariableDeclaration withModifiers(java.util.List<VariableModifier> modifiers) {
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }
  
  public LocalVariableDeclaration withType(LocalVariableType type) {
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }
  
  public LocalVariableDeclaration withDeclarators(java.util.List<VariableDeclarator> declarators) {
    return new LocalVariableDeclaration(modifiers, type, declarators);
  }
}