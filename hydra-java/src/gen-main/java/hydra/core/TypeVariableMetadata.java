// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * Metadata associated with a type variable, including typeclass constraints
 */
public class TypeVariableMetadata implements Serializable, Comparable<TypeVariableMetadata> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.TypeVariableMetadata");
  
  public static final hydra.core.Name FIELD_NAME_CLASSES = new hydra.core.Name("classes");
  
  /**
   * The set of typeclass constraints on this type variable
   */
  public final java.util.Set<hydra.core.Name> classes;
  
  public TypeVariableMetadata (java.util.Set<hydra.core.Name> classes) {
    this.classes = classes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeVariableMetadata)) {
      return false;
    }
    TypeVariableMetadata o = (TypeVariableMetadata) (other);
    return java.util.Objects.equals(
      this.classes,
      o.classes);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(classes);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeVariableMetadata other) {
    return Integer.compare(
      classes.hashCode(),
      other.classes.hashCode());
  }
}
