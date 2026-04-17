// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Complete query with path algebra and result projection
 */
public class QueryExpression implements Serializable, Comparable<QueryExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.QueryExpression");

  public static final hydra.core.Name PATH_EXPRESSION = new hydra.core.Name("pathExpression");

  public static final hydra.core.Name RESULT_PROJECTION = new hydra.core.Name("resultProjection");

  public final com.gdblab.pathAlgebra.expressions.PathExpression pathExpression;

  public final hydra.util.Maybe<com.gdblab.pathAlgebra.expressions.ResultProjection> resultProjection;

  public QueryExpression (com.gdblab.pathAlgebra.expressions.PathExpression pathExpression, hydra.util.Maybe<com.gdblab.pathAlgebra.expressions.ResultProjection> resultProjection) {
    this.pathExpression = pathExpression;
    this.resultProjection = resultProjection;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QueryExpression)) {
      return false;
    }
    QueryExpression o = (QueryExpression) other;
    return java.util.Objects.equals(
      this.pathExpression,
      o.pathExpression) && java.util.Objects.equals(
      this.resultProjection,
      o.resultProjection);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pathExpression) + 3 * java.util.Objects.hashCode(resultProjection);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QueryExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pathExpression,
      other.pathExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      resultProjection,
      other.resultProjection);
  }

  public QueryExpression withPathExpression(com.gdblab.pathAlgebra.expressions.PathExpression pathExpression) {
    return new QueryExpression(pathExpression, resultProjection);
  }

  public QueryExpression withResultProjection(hydra.util.Maybe<com.gdblab.pathAlgebra.expressions.ResultProjection> resultProjection) {
    return new QueryExpression(pathExpression, resultProjection);
  }
}
