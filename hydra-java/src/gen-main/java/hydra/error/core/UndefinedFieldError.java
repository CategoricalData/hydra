// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A reference to a field that does not exist in the given type
 */
public class UndefinedFieldError implements Serializable, Comparable<UndefinedFieldError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UndefinedFieldError");

  public static final hydra.core.Name FIELD_NAME = new hydra.core.Name("fieldName");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  /**
   * The name of the undefined field
   */
  public final hydra.core.Name fieldName;

  /**
   * The name of the type in which the field was expected
   */
  public final hydra.core.Name typeName;

  public UndefinedFieldError (hydra.core.Name fieldName, hydra.core.Name typeName) {
    this.fieldName = fieldName;
    this.typeName = typeName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedFieldError)) {
      return false;
    }
    UndefinedFieldError o = (UndefinedFieldError) other;
    return java.util.Objects.equals(
      this.fieldName,
      o.fieldName) && java.util.Objects.equals(
      this.typeName,
      o.typeName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fieldName) + 3 * java.util.Objects.hashCode(typeName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UndefinedFieldError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      fieldName,
      other.fieldName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      typeName,
      other.typeName);
  }

  public UndefinedFieldError withFieldName(hydra.core.Name fieldName) {
    return new UndefinedFieldError(fieldName, typeName);
  }

  public UndefinedFieldError withTypeName(hydra.core.Name typeName) {
    return new UndefinedFieldError(fieldName, typeName);
  }
}
