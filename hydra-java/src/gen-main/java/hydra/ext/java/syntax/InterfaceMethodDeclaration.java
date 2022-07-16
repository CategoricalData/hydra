package hydra.ext.java.syntax;

public class InterfaceMethodDeclaration {
  public final java.util.List<InterfaceMethodModifier> modifiers;
  
  public final MethodHeader header;
  
  public final MethodBody body;
  
  public InterfaceMethodDeclaration (java.util.List<InterfaceMethodModifier> modifiers, MethodHeader header, MethodBody body) {
    this.modifiers = modifiers;
    this.header = header;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceMethodDeclaration)) {
      return false;
    }
    InterfaceMethodDeclaration o = (InterfaceMethodDeclaration) (other);
    return modifiers.equals(o.modifiers) && header.equals(o.header) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * header.hashCode() + 5 * body.hashCode();
  }
  
  public InterfaceMethodDeclaration withModifiers(java.util.List<InterfaceMethodModifier> modifiers) {
    return new InterfaceMethodDeclaration(modifiers, header, body);
  }
  
  public InterfaceMethodDeclaration withHeader(MethodHeader header) {
    return new InterfaceMethodDeclaration(modifiers, header, body);
  }
  
  public InterfaceMethodDeclaration withBody(MethodBody body) {
    return new InterfaceMethodDeclaration(modifiers, header, body);
  }
}