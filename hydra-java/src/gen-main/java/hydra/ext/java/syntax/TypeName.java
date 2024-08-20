// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TypeName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.TypeName");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIER = new hydra.core.Name("qualifier");
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.PackageOrTypeName> qualifier;
  
  public TypeName (hydra.ext.java.syntax.TypeIdentifier identifier, hydra.util.Opt<hydra.ext.java.syntax.PackageOrTypeName> qualifier) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((qualifier));
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
  
  public TypeName withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new TypeName(identifier, qualifier);
  }
  
  public TypeName withQualifier(hydra.util.Opt<hydra.ext.java.syntax.PackageOrTypeName> qualifier) {
    java.util.Objects.requireNonNull((qualifier));
    return new TypeName(identifier, qualifier);
  }
}
