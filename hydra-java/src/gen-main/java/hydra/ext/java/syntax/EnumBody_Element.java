package hydra.ext.java.syntax;

public class EnumBody_Element {
  public final java.util.List<EnumConstant> constants;
  
  public final java.util.List<ClassBodyDeclaration> bodyDeclarations;
  
  public EnumBody_Element (java.util.List<EnumConstant> constants, java.util.List<ClassBodyDeclaration> bodyDeclarations) {
    this.constants = constants;
    this.bodyDeclarations = bodyDeclarations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumBody_Element)) {
      return false;
    }
    EnumBody_Element o = (EnumBody_Element) (other);
    return constants.equals(o.constants) && bodyDeclarations.equals(o.bodyDeclarations);
  }
  
  @Override
  public int hashCode() {
    return 2 * constants.hashCode() + 3 * bodyDeclarations.hashCode();
  }
  
  public EnumBody_Element withConstants(java.util.List<EnumConstant> constants) {
    return new EnumBody_Element(constants, bodyDeclarations);
  }
  
  public EnumBody_Element withBodyDeclarations(java.util.List<ClassBodyDeclaration> bodyDeclarations) {
    return new EnumBody_Element(constants, bodyDeclarations);
  }
}