// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnumBody_Element implements Serializable, Comparable<EnumBody_Element> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.EnumBody_Element");

  public static final hydra.core.Name CONSTANTS = new hydra.core.Name("constants");

  public static final hydra.core.Name BODY_DECLARATIONS = new hydra.core.Name("bodyDeclarations");

  public final hydra.util.ConsList<hydra.ext.java.syntax.EnumConstant> constants;

  public final hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration> bodyDeclarations;

  public EnumBody_Element (hydra.util.ConsList<hydra.ext.java.syntax.EnumConstant> constants, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration> bodyDeclarations) {
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
    cmp = ((Comparable) constants).compareTo(other.constants);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) bodyDeclarations).compareTo(other.bodyDeclarations);
  }

  public EnumBody_Element withConstants(hydra.util.ConsList<hydra.ext.java.syntax.EnumConstant> constants) {
    return new EnumBody_Element(constants, bodyDeclarations);
  }

  public EnumBody_Element withBodyDeclarations(hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration> bodyDeclarations) {
    return new EnumBody_Element(constants, bodyDeclarations);
  }
}
