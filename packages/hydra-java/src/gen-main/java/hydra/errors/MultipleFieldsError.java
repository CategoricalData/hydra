// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * Multiple fields with the same name were found in a record
 */
public class MultipleFieldsError implements Serializable, Comparable<MultipleFieldsError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.MultipleFieldsError");

  public static final hydra.core.Name FIELD_NAME = new hydra.core.Name("fieldName");

  /**
   * The field name which appeared more than once
   */
  public final hydra.core.Name fieldName;

  public MultipleFieldsError (hydra.core.Name fieldName) {
    this.fieldName = fieldName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultipleFieldsError)) {
      return false;
    }
    MultipleFieldsError o = (MultipleFieldsError) other;
    return java.util.Objects.equals(
      this.fieldName,
      o.fieldName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fieldName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MultipleFieldsError other) {
    return hydra.util.Comparing.compare(
      fieldName,
      other.fieldName);
  }
}
