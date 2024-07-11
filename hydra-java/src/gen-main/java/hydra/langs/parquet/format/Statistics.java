// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Statistics per row group and per page. All fields are optional.
 */
public class Statistics implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.Statistics");
  
  public final hydra.util.Opt<java.math.BigInteger> nullCount;
  
  public final hydra.util.Opt<java.math.BigInteger> distinctCount;
  
  public final hydra.util.Opt<String> maxValue;
  
  public final hydra.util.Opt<String> minValue;
  
  public Statistics (hydra.util.Opt<java.math.BigInteger> nullCount, hydra.util.Opt<java.math.BigInteger> distinctCount, hydra.util.Opt<String> maxValue, hydra.util.Opt<String> minValue) {
    java.util.Objects.requireNonNull((nullCount));
    java.util.Objects.requireNonNull((distinctCount));
    java.util.Objects.requireNonNull((maxValue));
    java.util.Objects.requireNonNull((minValue));
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
  
  public Statistics withNullCount(hydra.util.Opt<java.math.BigInteger> nullCount) {
    java.util.Objects.requireNonNull((nullCount));
    return new Statistics(nullCount, distinctCount, maxValue, minValue);
  }
  
  public Statistics withDistinctCount(hydra.util.Opt<java.math.BigInteger> distinctCount) {
    java.util.Objects.requireNonNull((distinctCount));
    return new Statistics(nullCount, distinctCount, maxValue, minValue);
  }
  
  public Statistics withMaxValue(hydra.util.Opt<String> maxValue) {
    java.util.Objects.requireNonNull((maxValue));
    return new Statistics(nullCount, distinctCount, maxValue, minValue);
  }
  
  public Statistics withMinValue(hydra.util.Opt<String> minValue) {
    java.util.Objects.requireNonNull((minValue));
    return new Statistics(nullCount, distinctCount, maxValue, minValue);
  }
}