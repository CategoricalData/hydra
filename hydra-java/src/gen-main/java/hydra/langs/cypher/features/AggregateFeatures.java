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
    java.util.Objects.requireNonNull((avg));
    java.util.Objects.requireNonNull((collect));
    java.util.Objects.requireNonNull((count));
    java.util.Objects.requireNonNull((max));
    java.util.Objects.requireNonNull((min));
    java.util.Objects.requireNonNull((percentileCont));
    java.util.Objects.requireNonNull((percentileDisc));
    java.util.Objects.requireNonNull((stdev));
    java.util.Objects.requireNonNull((sum));
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
    java.util.Objects.requireNonNull((avg));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withCollect(Boolean collect) {
    java.util.Objects.requireNonNull((collect));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withCount(Boolean count) {
    java.util.Objects.requireNonNull((count));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withMax(Boolean max) {
    java.util.Objects.requireNonNull((max));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withMin(Boolean min) {
    java.util.Objects.requireNonNull((min));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withPercentileCont(Boolean percentileCont) {
    java.util.Objects.requireNonNull((percentileCont));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withPercentileDisc(Boolean percentileDisc) {
    java.util.Objects.requireNonNull((percentileDisc));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withStdev(Boolean stdev) {
    java.util.Objects.requireNonNull((stdev));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
  
  public AggregateFeatures withSum(Boolean sum) {
    java.util.Objects.requireNonNull((sum));
    return new AggregateFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, sum);
  }
}