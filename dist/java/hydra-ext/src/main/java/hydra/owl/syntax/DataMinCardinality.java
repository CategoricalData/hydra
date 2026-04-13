// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class DataMinCardinality implements Serializable, Comparable<DataMinCardinality> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.DataMinCardinality");

  public static final hydra.core.Name BOUND = new hydra.core.Name("bound");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name RANGE = new hydra.core.Name("range");

  public final java.math.BigInteger bound;

  public final hydra.owl.syntax.DataPropertyExpression property;

  public final java.util.List<hydra.owl.syntax.DataRange> range;

  public DataMinCardinality (java.math.BigInteger bound, hydra.owl.syntax.DataPropertyExpression property, java.util.List<hydra.owl.syntax.DataRange> range) {
    this.bound = bound;
    this.property = property;
    this.range = range;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataMinCardinality)) {
      return false;
    }
    DataMinCardinality o = (DataMinCardinality) other;
    return this.bound.compareTo(o.bound) == 0 && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.range,
      o.range);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bound) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(range);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DataMinCardinality other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      bound,
      other.bound);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      range,
      other.range);
  }

  public DataMinCardinality withBound(java.math.BigInteger bound) {
    return new DataMinCardinality(bound, property, range);
  }

  public DataMinCardinality withProperty(hydra.owl.syntax.DataPropertyExpression property) {
    return new DataMinCardinality(bound, property, range);
  }

  public DataMinCardinality withRange(java.util.List<hydra.owl.syntax.DataRange> range) {
    return new DataMinCardinality(bound, property, range);
  }
}
