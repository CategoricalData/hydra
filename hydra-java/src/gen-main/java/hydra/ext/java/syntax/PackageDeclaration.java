// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class PackageDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.PackageDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIERS = new hydra.core.Name("identifiers");
  
  public final java.util.List<hydra.ext.java.syntax.PackageModifier> modifiers;
  
  public final java.util.List<hydra.ext.java.syntax.Identifier> identifiers;
  
  public PackageDeclaration (java.util.List<hydra.ext.java.syntax.PackageModifier> modifiers, java.util.List<hydra.ext.java.syntax.Identifier> identifiers) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((identifiers));
    this.modifiers = modifiers;
    this.identifiers = identifiers;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PackageDeclaration)) {
      return false;
    }
    PackageDeclaration o = (PackageDeclaration) (other);
    return modifiers.equals(o.modifiers) && identifiers.equals(o.identifiers);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifiers.hashCode();
  }
  
  public PackageDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.PackageModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new PackageDeclaration(modifiers, identifiers);
  }
  
  public PackageDeclaration withIdentifiers(java.util.List<hydra.ext.java.syntax.Identifier> identifiers) {
    java.util.Objects.requireNonNull((identifiers));
    return new PackageDeclaration(modifiers, identifiers);
  }
}
