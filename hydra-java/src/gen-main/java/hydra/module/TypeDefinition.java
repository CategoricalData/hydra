// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A type-level definition, including a name and the type
 */
public class TypeDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.module.TypeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The name of the type
   */
  public final hydra.core.Name name;
  
  /**
   * The type being defined
   */
  public final hydra.core.Type type;
  
  public TypeDefinition (hydra.core.Name name, hydra.core.Type type) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((type));
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeDefinition)) {
      return false;
    }
    TypeDefinition o = (TypeDefinition) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public TypeDefinition withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new TypeDefinition(name, type);
  }
  
  public TypeDefinition withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypeDefinition(name, type);
  }
}
