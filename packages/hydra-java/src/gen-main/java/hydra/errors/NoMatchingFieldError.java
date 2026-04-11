// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * No field with the expected name was present
 */
public class NoMatchingFieldError implements Serializable, Comparable<NoMatchingFieldError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.NoMatchingFieldError");

  public static final hydra.core.Name FIELD_NAME = new hydra.core.Name("fieldName");

  /**
   * The field name which was not found
   */
  public final hydra.core.Name fieldName;

  public NoMatchingFieldError (hydra.core.Name fieldName) {
    this.fieldName = fieldName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoMatchingFieldError)) {
      return false;
    }
    NoMatchingFieldError o = (NoMatchingFieldError) other;
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
  public int compareTo(NoMatchingFieldError other) {
    return hydra.util.Comparing.compare(
      fieldName,
      other.fieldName);
  }
}
