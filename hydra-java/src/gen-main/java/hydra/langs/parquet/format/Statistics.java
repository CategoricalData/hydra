// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Statistics per row group and per page. All fields are optional.
 */
public class Statistics implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.Statistics");
  
  public final java.util.Optional<java.math.BigInteger> nullCount;
  
  public final java.util.Optional<java.math.BigInteger> distinctCount;
  
  public final java.util.Optional<String> maxValue;
  
  public final java.util.Optional<String> minValue;
  
  public Statistics (java.util.Optional<java.math.BigInteger> nullCount, java.util.Optional<java.math.BigInteger> distinctCount, java.util.Optional<String> maxValue, java.util.Optional<String> minValue) {
    if (nullCount == null) {
      throw new IllegalArgumentException("null value for 'nullCount' argument");
    }
    if (distinctCount == null) {
      throw new IllegalArgumentException("null value for 'distinctCount' argument");
    }
    if (maxValue == null) {
      throw new IllegalArgumentException("null value for 'maxValue' argument");
    }
    if (minValue == null) {
      throw new IllegalArgumentException("null value for 'minValue' argument");
    }
    this.nullCount = nullCount;
    this.distinctCount = distinctCount;
    this.maxValue = maxValue;
    this.minValue = minValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Statistics)) {
      return false;
    }
    Statistics o = (Statistics) (other);
    return nullCount.equals(o.nullCount) && distinctCount.equals(o.distinctCount) && maxValue.equals(o.maxValue) && minValue.equals(o.minValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * nullCount.hashCode() + 3 * distinctCount.hashCode() + 5 * maxValue.hashCode() + 7 * minValue.hashCode();
  }
  
  public Statistics withNullCount(java.util.Optional<java.math.BigInteger> nullCount) {
    if (nullCount == null) {
      throw new IllegalArgumentException("null value for 'nullCount' argument");
    }
    return new Statistics(nullCount, distinctCount, maxValue, minValue);
  }
  
  public Statistics withDistinctCount(java.util.Optional<java.math.BigInteger> distinctCount) {
    if (distinctCount == null) {
      throw new IllegalArgumentException("null value for 'distinctCount' argument");
    }
    return new Statistics(nullCount, distinctCount, maxValue, minValue);
  }
  
  public Statistics withMaxValue(java.util.Optional<String> maxValue) {
    if (maxValue == null) {
      throw new IllegalArgumentException("null value for 'maxValue' argument");
    }
    return new Statistics(nullCount, distinctCount, maxValue, minValue);
  }
  
  public Statistics withMinValue(java.util.Optional<String> minValue) {
    if (minValue == null) {
      throw new IllegalArgumentException("null value for 'minValue' argument");
    }
    return new Statistics(nullCount, distinctCount, maxValue, minValue);
  }
}