// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SortSpecification implements Serializable, Comparable<SortSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SortSpecification");

  public static final hydra.core.Name SORT_KEY = new hydra.core.Name("sortKey");

  public static final hydra.core.Name ORDERING = new hydra.core.Name("ordering");

  public static final hydra.core.Name NULL_ORDERING = new hydra.core.Name("nullOrdering");

  public final openGql.grammar.ValueExpression sortKey;

  public final hydra.util.Maybe<openGql.grammar.OrderingSpecification> ordering;

  public final hydra.util.Maybe<openGql.grammar.NullOrdering> nullOrdering;

  public SortSpecification (openGql.grammar.ValueExpression sortKey, hydra.util.Maybe<openGql.grammar.OrderingSpecification> ordering, hydra.util.Maybe<openGql.grammar.NullOrdering> nullOrdering) {
    this.sortKey = sortKey;
    this.ordering = ordering;
    this.nullOrdering = nullOrdering;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SortSpecification)) {
      return false;
    }
    SortSpecification o = (SortSpecification) other;
    return java.util.Objects.equals(
      this.sortKey,
      o.sortKey) && java.util.Objects.equals(
      this.ordering,
      o.ordering) && java.util.Objects.equals(
      this.nullOrdering,
      o.nullOrdering);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(sortKey) + 3 * java.util.Objects.hashCode(ordering) + 5 * java.util.Objects.hashCode(nullOrdering);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SortSpecification other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      sortKey,
      other.sortKey);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      ordering,
      other.ordering);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nullOrdering,
      other.nullOrdering);
  }

  public SortSpecification withSortKey(openGql.grammar.ValueExpression sortKey) {
    return new SortSpecification(sortKey, ordering, nullOrdering);
  }

  public SortSpecification withOrdering(hydra.util.Maybe<openGql.grammar.OrderingSpecification> ordering) {
    return new SortSpecification(sortKey, ordering, nullOrdering);
  }

  public SortSpecification withNullOrdering(hydra.util.Maybe<openGql.grammar.NullOrdering> nullOrdering) {
    return new SortSpecification(sortKey, ordering, nullOrdering);
  }
}
