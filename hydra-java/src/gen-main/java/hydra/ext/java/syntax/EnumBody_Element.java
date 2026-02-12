// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnumBody_Element implements Serializable, Comparable<EnumBody_Element> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.EnumBody_Element");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANTS = new hydra.core.Name("constants");
  
  public static final hydra.core.Name FIELD_NAME_BODY_DECLARATIONS = new hydra.core.Name("bodyDeclarations");
  
  public final java.util.List<hydra.ext.java.syntax.EnumConstant> constants;
  
  public final java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration> bodyDeclarations;
  
  public EnumBody_Element (java.util.List<hydra.ext.java.syntax.EnumConstant> constants, java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration> bodyDeclarations) {
    this.constants = constants;
    this.bodyDeclarations = bodyDeclarations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumBody_Element)) {
      return false;
    }
    EnumBody_Element o = (EnumBody_Element) other;
    return java.util.Objects.equals(
      this.constants,
      o.constants) && java.util.Objects.equals(
      this.bodyDeclarations,
      o.bodyDeclarations);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(constants) + 3 * java.util.Objects.hashCode(bodyDeclarations);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EnumBody_Element other) {
    int cmp = 0;
    cmp = Integer.compare(
      constants.hashCode(),
      other.constants.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      bodyDeclarations.hashCode(),
      other.bodyDeclarations.hashCode());
  }
  
  public EnumBody_Element withConstants(java.util.List<hydra.ext.java.syntax.EnumConstant> constants) {
    return new EnumBody_Element(constants, bodyDeclarations);
  }
  
  public EnumBody_Element withBodyDeclarations(java.util.List<hydra.ext.java.syntax.ClassBodyDeclaration> bodyDeclarations) {
    return new EnumBody_Element(constants, bodyDeclarations);
  }
}
