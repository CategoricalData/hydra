// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A type-level definition, including a name and the type
 */
public class TypeDefinition implements Serializable, Comparable<TypeDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.module.TypeDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The name of the type
   */
  public final hydra.core.Name name;

  /**
   * The type being defined
   */
  public final hydra.core.Type type;

  public TypeDefinition (hydra.core.Name name, hydra.core.Type type) {
    this.name = name;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeDefinition)) {
      return false;
    }
    TypeDefinition o = (TypeDefinition) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public TypeDefinition withName(hydra.core.Name name) {
    return new TypeDefinition(name, type);
  }

  public TypeDefinition withType(hydra.core.Type type) {
    return new TypeDefinition(name, type);
  }
}
