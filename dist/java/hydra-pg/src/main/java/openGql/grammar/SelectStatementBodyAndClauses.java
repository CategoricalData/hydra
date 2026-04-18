// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SelectStatementBodyAndClauses implements Serializable, Comparable<SelectStatementBodyAndClauses> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SelectStatementBodyAndClauses");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name WHERE = new hydra.core.Name("where");

  public static final hydra.core.Name GROUP_BY = new hydra.core.Name("groupBy");

  public static final hydra.core.Name HAVING = new hydra.core.Name("having");

  public static final hydra.core.Name ORDER_BY = new hydra.core.Name("orderBy");

  public static final hydra.core.Name OFFSET = new hydra.core.Name("offset");

  public static final hydra.core.Name LIMIT = new hydra.core.Name("limit");

  public final openGql.grammar.SelectStatementBody body;

  public final hydra.util.Maybe<openGql.grammar.ValueExpression> where;

  public final hydra.util.Maybe<openGql.grammar.GroupingElementList> groupBy;

  public final hydra.util.Maybe<openGql.grammar.ValueExpression> having;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.SortSpecification>> orderBy;

  public final hydra.util.Maybe<openGql.grammar.OffsetClause> offset;

  public final hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit;

  public SelectStatementBodyAndClauses (openGql.grammar.SelectStatementBody body, hydra.util.Maybe<openGql.grammar.ValueExpression> where, hydra.util.Maybe<openGql.grammar.GroupingElementList> groupBy, hydra.util.Maybe<openGql.grammar.ValueExpression> having, hydra.util.Maybe<java.util.List<openGql.grammar.SortSpecification>> orderBy, hydra.util.Maybe<openGql.grammar.OffsetClause> offset, hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit) {
    this.body = body;
    this.where = where;
    this.groupBy = groupBy;
    this.having = having;
    this.orderBy = orderBy;
    this.offset = offset;
    this.limit = limit;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelectStatementBodyAndClauses)) {
      return false;
    }
    SelectStatementBodyAndClauses o = (SelectStatementBodyAndClauses) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.where,
      o.where) && java.util.Objects.equals(
      this.groupBy,
      o.groupBy) && java.util.Objects.equals(
      this.having,
      o.having) && java.util.Objects.equals(
      this.orderBy,
      o.orderBy) && java.util.Objects.equals(
      this.offset,
      o.offset) && java.util.Objects.equals(
      this.limit,
      o.limit);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(where) + 5 * java.util.Objects.hashCode(groupBy) + 7 * java.util.Objects.hashCode(having) + 11 * java.util.Objects.hashCode(orderBy) + 13 * java.util.Objects.hashCode(offset) + 17 * java.util.Objects.hashCode(limit);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SelectStatementBodyAndClauses other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      where,
      other.where);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      groupBy,
      other.groupBy);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      having,
      other.having);
    if (cmp != 0) {
      return cmp;
    }
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

  public SelectStatementBodyAndClauses withBody(openGql.grammar.SelectStatementBody body) {
    return new SelectStatementBodyAndClauses(body, where, groupBy, having, orderBy, offset, limit);
  }

  public SelectStatementBodyAndClauses withWhere(hydra.util.Maybe<openGql.grammar.ValueExpression> where) {
    return new SelectStatementBodyAndClauses(body, where, groupBy, having, orderBy, offset, limit);
  }

  public SelectStatementBodyAndClauses withGroupBy(hydra.util.Maybe<openGql.grammar.GroupingElementList> groupBy) {
    return new SelectStatementBodyAndClauses(body, where, groupBy, having, orderBy, offset, limit);
  }

  public SelectStatementBodyAndClauses withHaving(hydra.util.Maybe<openGql.grammar.ValueExpression> having) {
    return new SelectStatementBodyAndClauses(body, where, groupBy, having, orderBy, offset, limit);
  }

  public SelectStatementBodyAndClauses withOrderBy(hydra.util.Maybe<java.util.List<openGql.grammar.SortSpecification>> orderBy) {
    return new SelectStatementBodyAndClauses(body, where, groupBy, having, orderBy, offset, limit);
  }

  public SelectStatementBodyAndClauses withOffset(hydra.util.Maybe<openGql.grammar.OffsetClause> offset) {
    return new SelectStatementBodyAndClauses(body, where, groupBy, having, orderBy, offset, limit);
  }

  public SelectStatementBodyAndClauses withLimit(hydra.util.Maybe<openGql.grammar.NonNegativeIntegerSpecification> limit) {
    return new SelectStatementBodyAndClauses(body, where, groupBy, having, orderBy, offset, limit);
  }
}
