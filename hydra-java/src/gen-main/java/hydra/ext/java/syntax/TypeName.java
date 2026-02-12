// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TypeName implements Serializable, Comparable<TypeName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.TypeName");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIER = new hydra.core.Name("qualifier");
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.PackageOrTypeName> qualifier;
  
  public TypeName (hydra.ext.java.syntax.TypeIdentifier identifier, hydra.util.Maybe<hydra.ext.java.syntax.PackageOrTypeName> qualifier) {
    this.identifier = identifier;
    this.qualifier = qualifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeName)) {
      return false;
    }
    TypeName o = (TypeName) other;
    return java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.qualifier,
      o.qualifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(identifier) + 3 * java.util.Objects.hashCode(qualifier);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeName other) {
    int cmp = 0;
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      qualifier.hashCode(),
      other.qualifier.hashCode());
  }
  
  public TypeName withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new TypeName(identifier, qualifier);
  }
  
  public TypeName withQualifier(hydra.util.Maybe<hydra.ext.java.syntax.PackageOrTypeName> qualifier) {
    return new TypeName(identifier, qualifier);
  }
}
