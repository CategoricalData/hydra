// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class TypeName implements Serializable, Comparable<TypeName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.TypeName");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public static final hydra.core.Name QUALIFIER = new hydra.core.Name("qualifier");

  public final hydra.java.syntax.TypeIdentifier identifier;

  public final hydra.util.Maybe<hydra.java.syntax.PackageOrTypeName> qualifier;

  public TypeName (hydra.java.syntax.TypeIdentifier identifier, hydra.util.Maybe<hydra.java.syntax.PackageOrTypeName> qualifier) {
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
    cmp = hydra.util.Comparing.compare(
      identifier,
      other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      qualifier,
      other.qualifier);
  }

  public TypeName withIdentifier(hydra.java.syntax.TypeIdentifier identifier) {
    return new TypeName(identifier, qualifier);
  }

  public TypeName withQualifier(hydra.util.Maybe<hydra.java.syntax.PackageOrTypeName> qualifier) {
    return new TypeName(identifier, qualifier);
  }
}
