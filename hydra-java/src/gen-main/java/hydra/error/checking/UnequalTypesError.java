// Note: this is an automatically generated file. Do not edit.

package hydra.error.checking;

import java.io.Serializable;

/**
 * Multiple types that should all be equal but are not
 */
public class UnequalTypesError implements Serializable, Comparable<UnequalTypesError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.checking.UnequalTypesError");

  public static final hydra.core.Name TYPES = new hydra.core.Name("types");

  public static final hydra.core.Name DESCRIPTION = new hydra.core.Name("description");

  /**
   * The list of types that are not all equal
   */
  public final hydra.util.ConsList<hydra.core.Type> types;

  /**
   * A description of the context in which the types were expected to be equal
   */
  public final String description;

  public UnequalTypesError (hydra.util.ConsList<hydra.core.Type> types, String description) {
    this.types = types;
    this.description = description;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnequalTypesError)) {
      return false;
    }
    UnequalTypesError o = (UnequalTypesError) other;
    return java.util.Objects.equals(
      this.types,
      o.types) && java.util.Objects.equals(
      this.description,
      o.description);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(types) + 3 * java.util.Objects.hashCode(description);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnequalTypesError other) {
    int cmp = 0;
    cmp = ((Comparable) types).compareTo(other.types);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) description).compareTo(other.description);
  }

  public UnequalTypesError withTypes(hydra.util.ConsList<hydra.core.Type> types) {
    return new UnequalTypesError(types, description);
  }

  public UnequalTypesError withDescription(String description) {
    return new UnequalTypesError(types, description);
  }
}
