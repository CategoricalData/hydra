// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class TypeName implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.TypeName");
  
  public final hydra.langs.java.syntax.TypeIdentifier identifier;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.PackageOrTypeName> qualifier;
  
  public TypeName (hydra.langs.java.syntax.TypeIdentifier identifier, hydra.util.Opt<hydra.langs.java.syntax.PackageOrTypeName> qualifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    if (qualifier == null) {
      throw new IllegalArgumentException("null value for 'qualifier' argument");
    }
    this.identifier = identifier;
    this.qualifier = qualifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeName)) {
      return false;
    }
    TypeName o = (TypeName) (other);
    return identifier.equals(o.identifier) && qualifier.equals(o.qualifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * qualifier.hashCode();
  }
  
  public TypeName withIdentifier(hydra.langs.java.syntax.TypeIdentifier identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new TypeName(identifier, qualifier);
  }
  
  public TypeName withQualifier(hydra.util.Opt<hydra.langs.java.syntax.PackageOrTypeName> qualifier) {
    if (qualifier == null) {
      throw new IllegalArgumentException("null value for 'qualifier' argument");
    }
    return new TypeName(identifier, qualifier);
  }
}