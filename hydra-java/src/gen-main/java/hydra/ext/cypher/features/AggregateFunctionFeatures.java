// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Aggregate functions
 */
public class AggregateFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.AggregateFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_AVG = new hydra.core.Name("avg");
  
  public static final hydra.core.Name FIELD_NAME_COLLECT = new hydra.core.Name("collect");
  
  public static final hydra.core.Name FIELD_NAME_COUNT = new hydra.core.Name("count");
  
  public static final hydra.core.Name FIELD_NAME_MAX = new hydra.core.Name("max");
  
  public static final hydra.core.Name FIELD_NAME_MIN = new hydra.core.Name("min");
  
  public static final hydra.core.Name FIELD_NAME_PERCENTILE_CONT = new hydra.core.Name("percentileCont");
  
  public static final hydra.core.Name FIELD_NAME_PERCENTILE_DISC = new hydra.core.Name("percentileDisc");
  
  public static final hydra.core.Name FIELD_NAME_STDEV = new hydra.core.Name("stdev");
  
  public static final hydra.core.Name FIELD_NAME_STDEVP = new hydra.core.Name("stdevp");
  
  public static final hydra.core.Name FIELD_NAME_SUM = new hydra.core.Name("sum");
  
  /**
   * The avg() function / AVG. Returns the average of a set of DURATION values.; Returns the average of a set of FLOAT values.; Returns the average of a set of INTEGER values.
   */
  public final Boolean avg;
  
  /**
   * The collect() function / COLLECT. Returns a list containing the values returned by an expression.
   */
  public final Boolean collect;
  
  /**
   * The count() function / COUNT. Returns the number of values or rows.
   */
  public final Boolean count;
  
  /**
   * The max() function / MAX. Returns the maximum value in a set of values.
   */
  public final Boolean max;
  
  /**
   * The min() function / MIN. Returns the minimum value in a set of values.
   */
  public final Boolean min;
  
  /**
   * The percentileCont() function. Returns the percentile of a value over a group using linear interpolation.
   */
  public final Boolean percentileCont;
  
  /**
   * The percentileDisc() function. Returns the nearest FLOAT value to the given percentile over a group using a rounding method.; Returns the nearest INTEGER value to the given percentile over a group using a rounding method.
   */
  public final Boolean percentileDisc;
  
  /**
   * The stdev() function. Returns the standard deviation for the given value over a group for a sample of a population.
   */
  public final Boolean stdev;
  
  /**
   * The stdevp() function. Returns the standard deviation for the given value over a group for an entire population.
   */
  public final Boolean stdevp;
  
  /**
   * The sum() function / SUM. Returns the sum of a set of DURATION values.; Returns the sum of a set of FLOAT values.; Returns the sum of a set of INTEGER values.
   */
  public final Boolean sum;
  
  public AggregateFunctionFeatures (Boolean avg, Boolean collect, Boolean count, Boolean max, Boolean min, Boolean percentileCont, Boolean percentileDisc, Boolean stdev, Boolean stdevp, Boolean sum) {
    java.util.Objects.requireNonNull((avg));
    java.util.Objects.requireNonNull((collect));
    java.util.Objects.requireNonNull((count));
    java.util.Objects.requireNonNull((max));
    java.util.Objects.requireNonNull((min));
    java.util.Objects.requireNonNull((percentileCont));
    java.util.Objects.requireNonNull((percentileDisc));
    java.util.Objects.requireNonNull((stdev));
    java.util.Objects.requireNonNull((stdevp));
    java.util.Objects.requireNonNull((sum));
    this.avg = avg;
    this.collect = collect;
    this.count = count;
    this.max = max;
    this.min = min;
    this.percentileCont = percentileCont;
    this.percentileDisc = percentileDisc;
    this.stdev = stdev;
    this.stdevp = stdevp;
    this.sum = sum;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AggregateFunctionFeatures)) {
      return false;
    }
    AggregateFunctionFeatures o = (AggregateFunctionFeatures) (other);
    return avg.equals(o.avg) && collect.equals(o.collect) && count.equals(o.count) && max.equals(o.max) && min.equals(o.min) && percentileCont.equals(o.percentileCont) && percentileDisc.equals(o.percentileDisc) && stdev.equals(o.stdev) && stdevp.equals(o.stdevp) && sum.equals(o.sum);
  }
  
  @Override
  public int hashCode() {
    return 2 * avg.hashCode() + 3 * collect.hashCode() + 5 * count.hashCode() + 7 * max.hashCode() + 11 * min.hashCode() + 13 * percentileCont.hashCode() + 17 * percentileDisc.hashCode() + 19 * stdev.hashCode() + 23 * stdevp.hashCode() + 29 * sum.hashCode();
  }
  
  public AggregateFunctionFeatures withAvg(Boolean avg) {
    java.util.Objects.requireNonNull((avg));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withCollect(Boolean collect) {
    java.util.Objects.requireNonNull((collect));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withCount(Boolean count) {
    java.util.Objects.requireNonNull((count));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withMax(Boolean max) {
    java.util.Objects.requireNonNull((max));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withMin(Boolean min) {
    java.util.Objects.requireNonNull((min));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withPercentileCont(Boolean percentileCont) {
    java.util.Objects.requireNonNull((percentileCont));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withPercentileDisc(Boolean percentileDisc) {
    java.util.Objects.requireNonNull((percentileDisc));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withStdev(Boolean stdev) {
    java.util.Objects.requireNonNull((stdev));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withStdevp(Boolean stdevp) {
    java.util.Objects.requireNonNull((stdevp));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
  
  public AggregateFunctionFeatures withSum(Boolean sum) {
    java.util.Objects.requireNonNull((sum));
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
}
