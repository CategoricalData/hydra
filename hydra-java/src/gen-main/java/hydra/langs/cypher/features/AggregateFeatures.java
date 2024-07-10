// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for aggregation functions.
 */
public class AggregateFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.AggregateFeatures");
  
  /**
   * Whether to expect the avg() / AVG aggregate function.
   */
  public final Boolean avg;
  
  /**
   * Whether to expect the collect() / COLLECT aggregate function.
   */
  public final Boolean collect;
  
  /**
   * Whether to expect the count() / COUNT aggregate function.
   */
  public final Boolean count;
  
  /**
   * Whether to expect the max() / MAX aggregate function.
   */
  public final Boolean max;
  
  /**
   * Whether to expect the min() / MIN aggregate function.
   */
  public final Boolean min;
  
  /**
   * Whether to expect the percentileCont() function.
   */
  public final Boolean percentileCont;
  
  /**
   * Whether to expect the percentileDisc() function.
   */
  public final Boolean percentileDisc;
  
  /**
   * Whether to expect the stdev() function.
   */
  public final Boolean stdev;
  
  /**
   * Whether to expect the sum() / SUM aggregate function.
   */
  public final Boolean sum;
  
  public AggregateFeatures (Boolean avg, Boolean collect, Boolean count, Boolean max, Boolean min, Boolean percentileCont, Boolean percentileDisc, Boolean stdev, Boolean sum) {
    if (avg == null) {
      throw new IllegalArgumentException("null value for 'avg' argument");
    }
    if (collect == null) {
      throw new IllegalArgumentException("null value for 'collect' argument");
    }
    if (count == null) {
      throw new IllegalArgumentException("null value for 'count' argument");
    }
    if (max == null) {
      throw new IllegalArgumentException("null value for 'max' argument");
    }
    if (min == null) {
      throw new IllegalArgumentException("null value for 'min' argument");
    }
    if (percentileCont == null) {
      throw new IllegalArgumentException("null value for 'percentileCont' argument");
    }
    if (percentileDisc == null) {
      throw new IllegalArgumentException("null value for 'percentileDisc' argument");
    }
    if (stdev == null) {
      throw new IllegalArgumentException("null value for 'stdev' argument");
    }
    if (sum == null) {
      throw new IllegalArgumentException("null value for 'sum' argument");
    }
    this.avg = avg;
    this.collect = collect;
    this.count = count;
    this.max = max;
    this.min = min;
    this.percentileCont = percentileCont;
    this.percentileDisc = percentileDisc;
    this.stdev = stdev;
    this.sum = sum;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AggregateFeatures)) {
      return false;
    }
    AggregateFeatures o = (AggregateFeatures) (other);
    return avg.equals(o.avg) && collect.equals(o.collect) && count.equals(o.count) && max.equals(o.max) && min.equals(o.min) && percentileCont.equals(o.percentileCont) && percentileDisc.equals(o.percentileDisc) && stdev.equals(o.stdev) && sum.equals(o.sum);
  }
  
  @Override
  public int hashCode() {
    return 2 * avg.hashCode() + 3 * collect.hashCode() + 5 * count.hashCode() + 7 * max.hashCode() + 11 * min.hashCode() + 13 * percentileCont.hashCode() + 17 * percentileDisc.hashCode() + 19 * stdev.hashCode() + 23 * sum.hashCode();
  }
  
  public AggregateFeatures withAvg(Boolean avg) {
    if (avg == null) {
      throw new IllegalArgumentException("null value for 'avg' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withCollect(Boolean collect) {
    if (collect == null) {
      throw new IllegalArgumentException("null value for 'collect' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withCount(Boolean count) {
    if (count == null) {
      throw new IllegalArgumentException("null value for 'count' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withMax(Boolean max) {
    if (max == null) {
      throw new IllegalArgumentException("null value for 'max' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withMin(Boolean min) {
    if (min == null) {
      throw new IllegalArgumentException("null value for 'min' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withPercentileCont(Boolean percentileCont) {
    if (percentileCont == null) {
      throw new IllegalArgumentException("null value for 'percentileCont' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withPercentileDisc(Boolean percentileDisc) {
    if (percentileDisc == null) {
      throw new IllegalArgumentException("null value for 'percentileDisc' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withStdev(Boolean stdev) {
    if (stdev == null) {
      throw new IllegalArgumentException("null value for 'stdev' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withSum(Boolean sum) {
    if (sum == null) {
      throw new IllegalArgumentException("null value for 'sum' argument");
    }
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
}