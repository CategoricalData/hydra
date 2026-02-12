// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class StaticImportOnDemandDeclaration implements Serializable, Comparable<StaticImportOnDemandDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.StaticImportOnDemandDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.TypeName value;
  
  public StaticImportOnDemandDeclaration (hydra.ext.java.syntax.TypeName value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StaticImportOnDemandDeclaration)) {
      return false;
    }
    StaticImportOnDemandDeclaration o = (StaticImportOnDemandDeclaration) other;
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
  public int compareTo(StaticImportOnDemandDeclaration other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
