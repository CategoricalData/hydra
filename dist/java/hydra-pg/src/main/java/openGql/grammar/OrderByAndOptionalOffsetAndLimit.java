// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OrderByAndOptionalOffsetAndLimit implements Serializable, Comparable<OrderByAndOptionalOffsetAndLimit> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OrderByAndOptionalOffsetAndLimit");

  public static final hydra.core.Name ORDER_BY = new hydra.core.Name("orderBy");

  public static final hydra.core.Name OFFSET = new hydra.core.Name("offset");

  public static final hydra.core.Name LIMIT = new hydra.core.Name("limit");

  public final java.util.List<openGql.grammar.SortSpecification> orderBy;

  public final hydra.util.Maybe<openGql.grammar.OffsetClause> offset;

  public final hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit;

  public OrderByAndOptionalOffsetAndLimit (java.util.List<openGql.grammar.SortSpecification> orderBy, hydra.util.Maybe<openGql.grammar.OffsetClause> offset, hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit) {
    this.orderBy = orderBy;
    this.offset = offset;
    this.limit = limit;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrderByAndOptionalOffsetAndLimit)) {
      return false;
    }
    OrderByAndOptionalOffsetAndLimit o = (OrderByAndOptionalOffsetAndLimit) other;
    return java.util.Objects.equals(
      this.orderBy,
      o.orderBy) && java.util.Objects.equals(
      this.offset,
      o.offset) && java.util.Objects.equals(
      this.limit,
      o.limit);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(orderBy) + 3 * java.util.Objects.hashCode(offset) + 5 * java.util.Objects.hashCode(limit);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OrderByAndOptionalOffsetAndLimit other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      orderBy,
      other.orderBy);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      offset,
      other.offset);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      limit,
      other.limit);
  }

  public OrderByAndOptionalOffsetAndLimit withOrderBy(java.util.List<openGql.grammar.SortSpecification> orderBy) {
    return new OrderByAndOptionalOffsetAndLimit(orderBy, offset, limit);
  }

  public OrderByAndOptionalOffsetAndLimit withOffset(hydra.util.Maybe<openGql.grammar.OffsetClause> offset) {
    return new OrderByAndOptionalOffsetAndLimit(orderBy, offset, limit);
  }

  public OrderByAndOptionalOffsetAndLimit withLimit(hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit) {
    return new OrderByAndOptionalOffsetAndLimit(orderBy, offset, limit);
  }
}
