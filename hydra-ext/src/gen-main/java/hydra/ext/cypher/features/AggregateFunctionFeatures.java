// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Aggregate functions
 */
public class AggregateFunctionFeatures implements Serializable, Comparable<AggregateFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.AggregateFunctionFeatures");

  public static final hydra.core.Name AVG = new hydra.core.Name("avg");

  public static final hydra.core.Name COLLECT = new hydra.core.Name("collect");

  public static final hydra.core.Name COUNT = new hydra.core.Name("count");

  public static final hydra.core.Name MAX = new hydra.core.Name("max");

  public static final hydra.core.Name MIN = new hydra.core.Name("min");

  public static final hydra.core.Name PERCENTILE_CONT = new hydra.core.Name("percentileCont");

  public static final hydra.core.Name PERCENTILE_DISC = new hydra.core.Name("percentileDisc");

  public static final hydra.core.Name STDEV = new hydra.core.Name("stdev");

  public static final hydra.core.Name STDEVP = new hydra.core.Name("stdevp");

  public static final hydra.core.Name SUM = new hydra.core.Name("sum");

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
    AggregateFunctionFeatures o = (AggregateFunctionFeatures) other;
    return java.util.Objects.equals(
      this.avg,
      o.avg) && java.util.Objects.equals(
      this.collect,
      o.collect) && java.util.Objects.equals(
      this.count,
      o.count) && java.util.Objects.equals(
      this.max,
      o.max) && java.util.Objects.equals(
      this.min,
      o.min) && java.util.Objects.equals(
      this.percentileCont,
      o.percentileCont) && java.util.Objects.equals(
      this.percentileDisc,
      o.percentileDisc) && java.util.Objects.equals(
      this.stdev,
      o.stdev) && java.util.Objects.equals(
      this.stdevp,
      o.stdevp) && java.util.Objects.equals(
      this.sum,
      o.sum);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(avg) + 3 * java.util.Objects.hashCode(collect) + 5 * java.util.Objects.hashCode(count) + 7 * java.util.Objects.hashCode(max) + 11 * java.util.Objects.hashCode(min) + 13 * java.util.Objects.hashCode(percentileCont) + 17 * java.util.Objects.hashCode(percentileDisc) + 19 * java.util.Objects.hashCode(stdev) + 23 * java.util.Objects.hashCode(stdevp) + 29 * java.util.Objects.hashCode(sum);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AggregateFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) avg).compareTo(other.avg);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) collect).compareTo(other.collect);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) count).compareTo(other.count);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) max).compareTo(other.max);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) min).compareTo(other.min);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) percentileCont).compareTo(other.percentileCont);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) percentileDisc).compareTo(other.percentileDisc);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) stdev).compareTo(other.stdev);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) stdevp).compareTo(other.stdevp);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) sum).compareTo(other.sum);
  }

  public AggregateFunctionFeatures withAvg(Boolean avg) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withCollect(Boolean collect) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withCount(Boolean count) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withMax(Boolean max) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withMin(Boolean min) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withPercentileCont(Boolean percentileCont) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withPercentileDisc(Boolean percentileDisc) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withStdev(Boolean stdev) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withStdevp(Boolean stdevp) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }

  public AggregateFunctionFeatures withSum(Boolean sum) {
    return new AggregateFunctionFeatures(avg, collect, count, max, min, percentileCont, percentileDisc, stdev, stdevp, sum);
  }
}
