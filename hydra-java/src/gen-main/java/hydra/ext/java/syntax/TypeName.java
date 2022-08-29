package hydra.ext.java.syntax;

public class TypeName {
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final java.util.Optional<hydra.ext.java.syntax.PackageOrTypeName> qualifier;
  
  public TypeName (hydra.ext.java.syntax.TypeIdentifier identifier, java.util.Optional<hydra.ext.java.syntax.PackageOrTypeName> qualifier) {
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
    return new TypeName(identifier, qualifier);
  }
  
  public TypeName withQualifier(java.util.Optional<hydra.ext.java.syntax.PackageOrTypeName> qualifier) {
    return new TypeName(identifier, qualifier);
  }
}