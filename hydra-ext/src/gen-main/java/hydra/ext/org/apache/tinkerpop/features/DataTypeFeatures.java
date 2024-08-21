// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Base interface for features that relate to supporting different data types.
 */
public class DataTypeFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/features.DataTypeFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_BOOLEAN_ARRAY_VALUES = new hydra.core.Name("supportsBooleanArrayValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_BOOLEAN_VALUES = new hydra.core.Name("supportsBooleanValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_BYTE_ARRAY_VALUES = new hydra.core.Name("supportsByteArrayValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_BYTE_VALUES = new hydra.core.Name("supportsByteValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_DOUBLE_ARRAY_VALUES = new hydra.core.Name("supportsDoubleArrayValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_DOUBLE_VALUES = new hydra.core.Name("supportsDoubleValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_FLOAT_ARRAY_VALUES = new hydra.core.Name("supportsFloatArrayValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_FLOAT_VALUES = new hydra.core.Name("supportsFloatValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_INTEGER_ARRAY_VALUES = new hydra.core.Name("supportsIntegerArrayValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_INTEGER_VALUES = new hydra.core.Name("supportsIntegerValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_LONG_ARRAY_VALUES = new hydra.core.Name("supportsLongArrayValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_LONG_VALUES = new hydra.core.Name("supportsLongValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_MAP_VALUES = new hydra.core.Name("supportsMapValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_MIXED_LIST_VALUES = new hydra.core.Name("supportsMixedListValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_SERIALIZABLE_VALUES = new hydra.core.Name("supportsSerializableValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_STRING_ARRAY_VALUES = new hydra.core.Name("supportsStringArrayValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_STRING_VALUES = new hydra.core.Name("supportsStringValues");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_UNIFORM_LIST_VALUES = new hydra.core.Name("supportsUniformListValues");
  
  /**
   * Supports setting of an array of boolean values.
   */
  public final Boolean supportsBooleanArrayValues;
  
  /**
   * Supports setting of a boolean value.
   */
  public final Boolean supportsBooleanValues;
  
  /**
   * Supports setting of an array of byte values.
   */
  public final Boolean supportsByteArrayValues;
  
  /**
   * Supports setting of a byte value.
   */
  public final Boolean supportsByteValues;
  
  /**
   * Supports setting of an array of double values.
   */
  public final Boolean supportsDoubleArrayValues;
  
  /**
   * Supports setting of a double value.
   */
  public final Boolean supportsDoubleValues;
  
  /**
   * Supports setting of an array of float values.
   */
  public final Boolean supportsFloatArrayValues;
  
  /**
   * Supports setting of a float value.
   */
  public final Boolean supportsFloatValues;
  
  /**
   * Supports setting of an array of integer values.
   */
  public final Boolean supportsIntegerArrayValues;
  
  /**
   * Supports setting of a integer value.
   */
  public final Boolean supportsIntegerValues;
  
  /**
   * Supports setting of an array of long values.
   */
  public final Boolean supportsLongArrayValues;
  
  /**
   * Supports setting of a long value.
   */
  public final Boolean supportsLongValues;
  
  /**
   * Supports setting of a Map value.
   */
  public final Boolean supportsMapValues;
  
  /**
   * Supports setting of a List value.
   */
  public final Boolean supportsMixedListValues;
  
  /**
   * Supports setting of a Java serializable value.
   */
  public final Boolean supportsSerializableValues;
  
  /**
   * Supports setting of an array of string values.
   */
  public final Boolean supportsStringArrayValues;
  
  /**
   * Supports setting of a string value.
   */
  public final Boolean supportsStringValues;
  
  /**
   * Supports setting of a List value.
   */
  public final Boolean supportsUniformListValues;
  
  public DataTypeFeatures (Boolean supportsBooleanArrayValues, Boolean supportsBooleanValues, Boolean supportsByteArrayValues, Boolean supportsByteValues, Boolean supportsDoubleArrayValues, Boolean supportsDoubleValues, Boolean supportsFloatArrayValues, Boolean supportsFloatValues, Boolean supportsIntegerArrayValues, Boolean supportsIntegerValues, Boolean supportsLongArrayValues, Boolean supportsLongValues, Boolean supportsMapValues, Boolean supportsMixedListValues, Boolean supportsSerializableValues, Boolean supportsStringArrayValues, Boolean supportsStringValues, Boolean supportsUniformListValues) {
    java.util.Objects.requireNonNull((supportsBooleanArrayValues));
    java.util.Objects.requireNonNull((supportsBooleanValues));
    java.util.Objects.requireNonNull((supportsByteArrayValues));
    java.util.Objects.requireNonNull((supportsByteValues));
    java.util.Objects.requireNonNull((supportsDoubleArrayValues));
    java.util.Objects.requireNonNull((supportsDoubleValues));
    java.util.Objects.requireNonNull((supportsFloatArrayValues));
    java.util.Objects.requireNonNull((supportsFloatValues));
    java.util.Objects.requireNonNull((supportsIntegerArrayValues));
    java.util.Objects.requireNonNull((supportsIntegerValues));
    java.util.Objects.requireNonNull((supportsLongArrayValues));
    java.util.Objects.requireNonNull((supportsLongValues));
    java.util.Objects.requireNonNull((supportsMapValues));
    java.util.Objects.requireNonNull((supportsMixedListValues));
    java.util.Objects.requireNonNull((supportsSerializableValues));
    java.util.Objects.requireNonNull((supportsStringArrayValues));
    java.util.Objects.requireNonNull((supportsStringValues));
    java.util.Objects.requireNonNull((supportsUniformListValues));
    this.supportsBooleanArrayValues = supportsBooleanArrayValues;
    this.supportsBooleanValues = supportsBooleanValues;
    this.supportsByteArrayValues = supportsByteArrayValues;
    this.supportsByteValues = supportsByteValues;
    this.supportsDoubleArrayValues = supportsDoubleArrayValues;
    this.supportsDoubleValues = supportsDoubleValues;
    this.supportsFloatArrayValues = supportsFloatArrayValues;
    this.supportsFloatValues = supportsFloatValues;
    this.supportsIntegerArrayValues = supportsIntegerArrayValues;
    this.supportsIntegerValues = supportsIntegerValues;
    this.supportsLongArrayValues = supportsLongArrayValues;
    this.supportsLongValues = supportsLongValues;
    this.supportsMapValues = supportsMapValues;
    this.supportsMixedListValues = supportsMixedListValues;
    this.supportsSerializableValues = supportsSerializableValues;
    this.supportsStringArrayValues = supportsStringArrayValues;
    this.supportsStringValues = supportsStringValues;
    this.supportsUniformListValues = supportsUniformListValues;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataTypeFeatures)) {
      return false;
    }
    DataTypeFeatures o = (DataTypeFeatures) (other);
    return supportsBooleanArrayValues.equals(o.supportsBooleanArrayValues) && supportsBooleanValues.equals(o.supportsBooleanValues) && supportsByteArrayValues.equals(o.supportsByteArrayValues) && supportsByteValues.equals(o.supportsByteValues) && supportsDoubleArrayValues.equals(o.supportsDoubleArrayValues) && supportsDoubleValues.equals(o.supportsDoubleValues) && supportsFloatArrayValues.equals(o.supportsFloatArrayValues) && supportsFloatValues.equals(o.supportsFloatValues) && supportsIntegerArrayValues.equals(o.supportsIntegerArrayValues) && supportsIntegerValues.equals(o.supportsIntegerValues) && supportsLongArrayValues.equals(o.supportsLongArrayValues) && supportsLongValues.equals(o.supportsLongValues) && supportsMapValues.equals(o.supportsMapValues) && supportsMixedListValues.equals(o.supportsMixedListValues) && supportsSerializableValues.equals(o.supportsSerializableValues) && supportsStringArrayValues.equals(o.supportsStringArrayValues) && supportsStringValues.equals(o.supportsStringValues) && supportsUniformListValues.equals(o.supportsUniformListValues);
  }
  
  @Override
  public int hashCode() {
    return 2 * supportsBooleanArrayValues.hashCode() + 3 * supportsBooleanValues.hashCode() + 5 * supportsByteArrayValues.hashCode() + 7 * supportsByteValues.hashCode() + 11 * supportsDoubleArrayValues.hashCode() + 13 * supportsDoubleValues.hashCode() + 17 * supportsFloatArrayValues.hashCode() + 19 * supportsFloatValues.hashCode() + 23 * supportsIntegerArrayValues.hashCode() + 29 * supportsIntegerValues.hashCode() + 31 * supportsLongArrayValues.hashCode() + 37 * supportsLongValues.hashCode() + 41 * supportsMapValues.hashCode() + 43 * supportsMixedListValues.hashCode() + 47 * supportsSerializableValues.hashCode() + 53 * supportsStringArrayValues.hashCode() + 59 * supportsStringValues.hashCode() + 61 * supportsUniformListValues.hashCode();
  }
  
  public DataTypeFeatures withSupportsBooleanArrayValues(Boolean supportsBooleanArrayValues) {
    java.util.Objects.requireNonNull((supportsBooleanArrayValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsBooleanValues(Boolean supportsBooleanValues) {
    java.util.Objects.requireNonNull((supportsBooleanValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsByteArrayValues(Boolean supportsByteArrayValues) {
    java.util.Objects.requireNonNull((supportsByteArrayValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsByteValues(Boolean supportsByteValues) {
    java.util.Objects.requireNonNull((supportsByteValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsDoubleArrayValues(Boolean supportsDoubleArrayValues) {
    java.util.Objects.requireNonNull((supportsDoubleArrayValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsDoubleValues(Boolean supportsDoubleValues) {
    java.util.Objects.requireNonNull((supportsDoubleValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsFloatArrayValues(Boolean supportsFloatArrayValues) {
    java.util.Objects.requireNonNull((supportsFloatArrayValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsFloatValues(Boolean supportsFloatValues) {
    java.util.Objects.requireNonNull((supportsFloatValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsIntegerArrayValues(Boolean supportsIntegerArrayValues) {
    java.util.Objects.requireNonNull((supportsIntegerArrayValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsIntegerValues(Boolean supportsIntegerValues) {
    java.util.Objects.requireNonNull((supportsIntegerValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsLongArrayValues(Boolean supportsLongArrayValues) {
    java.util.Objects.requireNonNull((supportsLongArrayValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsLongValues(Boolean supportsLongValues) {
    java.util.Objects.requireNonNull((supportsLongValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsMapValues(Boolean supportsMapValues) {
    java.util.Objects.requireNonNull((supportsMapValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsMixedListValues(Boolean supportsMixedListValues) {
    java.util.Objects.requireNonNull((supportsMixedListValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsSerializableValues(Boolean supportsSerializableValues) {
    java.util.Objects.requireNonNull((supportsSerializableValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsStringArrayValues(Boolean supportsStringArrayValues) {
    java.util.Objects.requireNonNull((supportsStringArrayValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsStringValues(Boolean supportsStringValues) {
    java.util.Objects.requireNonNull((supportsStringValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
  
  public DataTypeFeatures withSupportsUniformListValues(Boolean supportsUniformListValues) {
    java.util.Objects.requireNonNull((supportsUniformListValues));
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
}