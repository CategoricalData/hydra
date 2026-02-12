// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class PackageDeclaration implements Serializable, Comparable<PackageDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.PackageDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIERS = new hydra.core.Name("identifiers");
  
  public final java.util.List<hydra.ext.java.syntax.PackageModifier> modifiers;
  
  public final java.util.List<hydra.ext.java.syntax.Identifier> identifiers;
  
  public PackageDeclaration (java.util.List<hydra.ext.java.syntax.PackageModifier> modifiers, java.util.List<hydra.ext.java.syntax.Identifier> identifiers) {
    this.modifiers = modifiers;
    this.identifiers = identifiers;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PackageDeclaration)) {
      return false;
    }
    PackageDeclaration o = (PackageDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.identifiers,
      o.identifiers);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(identifiers);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PackageDeclaration other) {
    int cmp = 0;
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      identifiers.hashCode(),
      other.identifiers.hashCode());
  }
  
  public PackageDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.PackageModifier> modifiers) {
    return new PackageDeclaration(modifiers, identifiers);
  }
  
  public PackageDeclaration withIdentifiers(java.util.List<hydra.ext.java.syntax.Identifier> identifiers) {
    return new PackageDeclaration(modifiers, identifiers);
  }
}
