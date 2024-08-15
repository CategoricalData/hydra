// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class TypeImportOnDemandDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.TypeImportOnDemandDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.langs.java.syntax.PackageOrTypeName value;
  
  public TypeImportOnDemandDeclaration (hydra.langs.java.syntax.PackageOrTypeName value) {
    java.util.Objects.requireNonNull((value));
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