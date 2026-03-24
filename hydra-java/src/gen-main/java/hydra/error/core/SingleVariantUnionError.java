// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A union type with exactly one field; could be a wrapped type or record instead (optional)
 */
public class SingleVariantUnionError implements Serializable, Comparable<SingleVariantUnionError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.SingleVariantUnionError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name FIELD_NAME = new hydra.core.Name("fieldName");

  /**
   * The path to the single-variant union type
   */
  public final hydra.accessors.AccessorPath location;

  /**
   * The name of the single field
   */
  public final hydra.core.Name fieldName;

  public SingleVariantUnionError (hydra.accessors.AccessorPath location, hydra.core.Name fieldName) {
    this.location = location;
    this.fieldName = fieldName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleVariantUnionError)) {
      return false;
    }
    SingleVariantUnionError o = (SingleVariantUnionError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.fieldName,
      o.fieldName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(fieldName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SingleVariantUnionError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) fieldName).compareTo(other.fieldName);
  }

  public SingleVariantUnionError withLocation(hydra.accessors.AccessorPath location) {
    return new SingleVariantUnionError(location, fieldName);
  }

  public SingleVariantUnionError withFieldName(hydra.core.Name fieldName) {
    return new SingleVariantUnionError(location, fieldName);
  }
}
