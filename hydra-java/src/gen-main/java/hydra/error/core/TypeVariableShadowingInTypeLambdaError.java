// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type lambda parameter that shadows a type variable already in scope (optional)
 */
public class TypeVariableShadowingInTypeLambdaError implements Serializable, Comparable<TypeVariableShadowingInTypeLambdaError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the type lambda within the term
   */
  public final hydra.accessors.AccessorPath location;

  /**
   * The name of the shadowed type variable
   */
  public final hydra.core.Name name;

  public TypeVariableShadowingInTypeLambdaError (hydra.accessors.AccessorPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeVariableShadowingInTypeLambdaError)) {
      return false;
    }
    TypeVariableShadowingInTypeLambdaError o = (TypeVariableShadowingInTypeLambdaError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeVariableShadowingInTypeLambdaError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public TypeVariableShadowingInTypeLambdaError withLocation(hydra.accessors.AccessorPath location) {
    return new TypeVariableShadowingInTypeLambdaError(location, name);
  }

  public TypeVariableShadowingInTypeLambdaError withName(hydra.core.Name name) {
    return new TypeVariableShadowingInTypeLambdaError(location, name);
  }
}
