package hydra.ext.java.syntax;

public class TypeImportOnDemandDeclaration {
  public final hydra.ext.java.syntax.PackageOrTypeName value;
  
  public TypeImportOnDemandDeclaration (hydra.ext.java.syntax.PackageOrTypeName value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeImportOnDemandDeclaration)) {
      return false;
    }
    TypeImportOnDemandDeclaration o = (TypeImportOnDemandDeclaration) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}