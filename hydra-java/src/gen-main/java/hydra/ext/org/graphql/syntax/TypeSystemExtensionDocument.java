// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class TypeSystemExtensionDocument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.TypeSystemExtensionDocument");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.org.graphql.syntax.TypeSystemDefinitionOrExtension> value;
  
  public TypeSystemExtensionDocument (java.util.List<hydra.ext.org.graphql.syntax.TypeSystemDefinitionOrExtension> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSystemExtensionDocument)) {
      return false;
    }
    TypeSystemExtensionDocument o = (TypeSystemExtensionDocument) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}