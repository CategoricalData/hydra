// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class TypeSystemDocment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.TypeSystemDocment");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.langs.graphql.syntax.TypeSystemDefinition> value;
  
  public TypeSystemDocment (java.util.List<hydra.langs.graphql.syntax.TypeSystemDefinition> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSystemDocment)) {
      return false;
    }
    TypeSystemDocment o = (TypeSystemDocment) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}