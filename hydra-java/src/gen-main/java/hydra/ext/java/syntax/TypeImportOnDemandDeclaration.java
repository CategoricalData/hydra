// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TypeImportOnDemandDeclaration implements Serializable, Comparable<TypeImportOnDemandDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.TypeImportOnDemandDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.PackageOrTypeName value;
  
  public TypeImportOnDemandDeclaration (hydra.ext.java.syntax.PackageOrTypeName value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeImportOnDemandDeclaration)) {
      return false;
    }
    TypeImportOnDemandDeclaration o = (TypeImportOnDemandDeclaration) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeImportOnDemandDeclaration other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
