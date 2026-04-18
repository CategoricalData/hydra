// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.features;

import java.io.Serializable;

/**
 * Base interface for features that relate to supporting different data types.
 */
public class DataTypeFeatures implements Serializable, Comparable<DataTypeFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.features.DataTypeFeatures");

  public static final hydra.core.Name SUPPORTS_BOOLEAN_ARRAY_VALUES = new hydra.core.Name("supportsBooleanArrayValues");

  public static final hydra.core.Name SUPPORTS_BOOLEAN_VALUES = new hydra.core.Name("supportsBooleanValues");

  public static final hydra.core.Name SUPPORTS_BYTE_ARRAY_VALUES = new hydra.core.Name("supportsByteArrayValues");

  public static final hydra.core.Name SUPPORTS_BYTE_VALUES = new hydra.core.Name("supportsByteValues");

  public static final hydra.core.Name SUPPORTS_DOUBLE_ARRAY_VALUES = new hydra.core.Name("supportsDoubleArrayValues");

  public static final hydra.core.Name SUPPORTS_DOUBLE_VALUES = new hydra.core.Name("supportsDoubleValues");

  public static final hydra.core.Name SUPPORTS_FLOAT_ARRAY_VALUES = new hydra.core.Name("supportsFloatArrayValues");

  public static final hydra.core.Name SUPPORTS_FLOAT_VALUES = new hydra.core.Name("supportsFloatValues");

  public static final hydra.core.Name SUPPORTS_INTEGER_ARRAY_VALUES = new hydra.core.Name("supportsIntegerArrayValues");

  public static final hydra.core.Name SUPPORTS_INTEGER_VALUES = new hydra.core.Name("supportsIntegerValues");

  public static final hydra.core.Name SUPPORTS_LONG_ARRAY_VALUES = new hydra.core.Name("supportsLongArrayValues");

  public static final hydra.core.Name SUPPORTS_LONG_VALUES = new hydra.core.Name("supportsLongValues");

  public static final hydra.core.Name SUPPORTS_MAP_VALUES = new hydra.core.Name("supportsMapValues");

  public static final hydra.core.Name SUPPORTS_MIXED_LIST_VALUES = new hydra.core.Name("supportsMixedListValues");

  public static final hydra.core.Name SUPPORTS_SERIALIZABLE_VALUES = new hydra.core.Name("supportsSerializableValues");

  public static final hydra.core.Name SUPPORTS_STRING_ARRAY_VALUES = new hydra.core.Name("supportsStringArrayValues");

  public static final hydra.core.Name SUPPORTS_STRING_VALUES = new hydra.core.Name("supportsStringValues");

  public static final hydra.core.Name SUPPORTS_UNIFORM_LIST_VALUES = new hydra.core.Name("supportsUniformListValues");

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
    DataTypeFeatures o = (DataTypeFeatures) other;
    return java.util.Objects.equals(
      this.supportsBooleanArrayValues,
      o.supportsBooleanArrayValues) && java.util.Objects.equals(
      this.supportsBooleanValues,
      o.supportsBooleanValues) && java.util.Objects.equals(
      this.supportsByteArrayValues,
      o.supportsByteArrayValues) && java.util.Objects.equals(
      this.supportsByteValues,
      o.supportsByteValues) && java.util.Objects.equals(
      this.supportsDoubleArrayValues,
      o.supportsDoubleArrayValues) && java.util.Objects.equals(
      this.supportsDoubleValues,
      o.supportsDoubleValues) && java.util.Objects.equals(
      this.supportsFloatArrayValues,
      o.supportsFloatArrayValues) && java.util.Objects.equals(
      this.supportsFloatValues,
      o.supportsFloatValues) && java.util.Objects.equals(
      this.supportsIntegerArrayValues,
      o.supportsIntegerArrayValues) && java.util.Objects.equals(
      this.supportsIntegerValues,
      o.supportsIntegerValues) && java.util.Objects.equals(
      this.supportsLongArrayValues,
      o.supportsLongArrayValues) && java.util.Objects.equals(
      this.supportsLongValues,
      o.supportsLongValues) && java.util.Objects.equals(
      this.supportsMapValues,
      o.supportsMapValues) && java.util.Objects.equals(
      this.supportsMixedListValues,
      o.supportsMixedListValues) && java.util.Objects.equals(
      this.supportsSerializableValues,
      o.supportsSerializableValues) && java.util.Objects.equals(
      this.supportsStringArrayValues,
      o.supportsStringArrayValues) && java.util.Objects.equals(
      this.supportsStringValues,
      o.supportsStringValues) && java.util.Objects.equals(
      this.supportsUniformListValues,
      o.supportsUniformListValues);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(supportsBooleanArrayValues) + 3 * java.util.Objects.hashCode(supportsBooleanValues) + 5 * java.util.Objects.hashCode(supportsByteArrayValues) + 7 * java.util.Objects.hashCode(supportsByteValues) + 11 * java.util.Objects.hashCode(supportsDoubleArrayValues) + 13 * java.util.Objects.hashCode(supportsDoubleValues) + 17 * java.util.Objects.hashCode(supportsFloatArrayValues) + 19 * java.util.Objects.hashCode(supportsFloatValues) + 23 * java.util.Objects.hashCode(supportsIntegerArrayValues) + 29 * java.util.Objects.hashCode(supportsIntegerValues) + 31 * java.util.Objects.hashCode(supportsLongArrayValues) + 37 * java.util.Objects.hashCode(supportsLongValues) + 41 * java.util.Objects.hashCode(supportsMapValues) + 43 * java.util.Objects.hashCode(supportsMixedListValues) + 47 * java.util.Objects.hashCode(supportsSerializableValues) + 53 * java.util.Objects.hashCode(supportsStringArrayValues) + 59 * java.util.Objects.hashCode(supportsStringValues) + 61 * java.util.Objects.hashCode(supportsUniformListValues);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DataTypeFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      supportsBooleanArrayValues,
      other.supportsBooleanArrayValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsBooleanValues,
      other.supportsBooleanValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsByteArrayValues,
      other.supportsByteArrayValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsByteValues,
      other.supportsByteValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsDoubleArrayValues,
      other.supportsDoubleArrayValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsDoubleValues,
      other.supportsDoubleValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsFloatArrayValues,
      other.supportsFloatArrayValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsFloatValues,
      other.supportsFloatValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsIntegerArrayValues,
      other.supportsIntegerArrayValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsIntegerValues,
      other.supportsIntegerValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsLongArrayValues,
      other.supportsLongArrayValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsLongValues,
      other.supportsLongValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsMapValues,
      other.supportsMapValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsMixedListValues,
      other.supportsMixedListValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsSerializableValues,
      other.supportsSerializableValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsStringArrayValues,
      other.supportsStringArrayValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsStringValues,
      other.supportsStringValues);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      supportsUniformListValues,
      other.supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsBooleanArrayValues(Boolean supportsBooleanArrayValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsBooleanValues(Boolean supportsBooleanValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsByteArrayValues(Boolean supportsByteArrayValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsByteValues(Boolean supportsByteValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsDoubleArrayValues(Boolean supportsDoubleArrayValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsDoubleValues(Boolean supportsDoubleValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsFloatArrayValues(Boolean supportsFloatArrayValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsFloatValues(Boolean supportsFloatValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsIntegerArrayValues(Boolean supportsIntegerArrayValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsIntegerValues(Boolean supportsIntegerValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsLongArrayValues(Boolean supportsLongArrayValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsLongValues(Boolean supportsLongValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsMapValues(Boolean supportsMapValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsMixedListValues(Boolean supportsMixedListValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsSerializableValues(Boolean supportsSerializableValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsStringArrayValues(Boolean supportsStringArrayValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsStringValues(Boolean supportsStringValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }

  public DataTypeFeatures withSupportsUniformListValues(Boolean supportsUniformListValues) {
    return new DataTypeFeatures(supportsBooleanArrayValues, supportsBooleanValues, supportsByteArrayValues, supportsByteValues, supportsDoubleArrayValues, supportsDoubleValues, supportsFloatArrayValues, supportsFloatValues, supportsIntegerArrayValues, supportsIntegerValues, supportsLongArrayValues, supportsLongValues, supportsMapValues, supportsMixedListValues, supportsSerializableValues, supportsStringArrayValues, supportsStringValues, supportsUniformListValues);
  }
}
