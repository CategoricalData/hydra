// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class GraphPattern implements Serializable, Comparable<GraphPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphPattern");

  public static final hydra.core.Name MATCH_MODE = new hydra.core.Name("matchMode");

  public static final hydra.core.Name PATH_PATTERNS = new hydra.core.Name("pathPatterns");

  public static final hydra.core.Name KEEP_CLAUSE = new hydra.core.Name("keepClause");

  public static final hydra.core.Name WHERE_CLAUSE = new hydra.core.Name("whereClause");

  public final hydra.util.Maybe<openGql.grammar.MatchMode> matchMode;

  public final java.util.List<openGql.grammar.PathPattern> pathPatterns;

  public final hydra.util.Maybe<openGql.grammar.PathPatternPrefix> keepClause;

  public final hydra.util.Maybe<openGql.grammar.ValueExpression> whereClause;

  public GraphPattern (hydra.util.Maybe<openGql.grammar.MatchMode> matchMode, java.util.List<openGql.grammar.PathPattern> pathPatterns, hydra.util.Maybe<openGql.grammar.PathPatternPrefix> keepClause, hydra.util.Maybe<openGql.grammar.ValueExpression> whereClause) {
    this.matchMode = matchMode;
    this.pathPatterns = pathPatterns;
    this.keepClause = keepClause;
    this.whereClause = whereClause;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphPattern)) {
      return false;
    }
    GraphPattern o = (GraphPattern) other;
    return java.util.Objects.equals(
      this.matchMode,
      o.matchMode) && java.util.Objects.equals(
      this.pathPatterns,
      o.pathPatterns) && java.util.Objects.equals(
      this.keepClause,
      o.keepClause) && java.util.Objects.equals(
      this.whereClause,
      o.whereClause);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(matchMode) + 3 * java.util.Objects.hashCode(pathPatterns) + 5 * java.util.Objects.hashCode(keepClause) + 7 * java.util.Objects.hashCode(whereClause);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GraphPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      matchMode,
      other.matchMode);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      pathPatterns,
      other.pathPatterns);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      keepClause,
      other.keepClause);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      whereClause,
      other.whereClause);
  }

  public GraphPattern withMatchMode(hydra.util.Maybe<openGql.grammar.MatchMode> matchMode) {
    return new GraphPattern(matchMode, pathPatterns, keepClause, whereClause);
  }

  public GraphPattern withPathPatterns(java.util.List<openGql.grammar.PathPattern> pathPatterns) {
    return new GraphPattern(matchMode, pathPatterns, keepClause, whereClause);
  }

  public GraphPattern withKeepClause(hydra.util.Maybe<openGql.grammar.PathPatternPrefix> keepClause) {
    return new GraphPattern(matchMode, pathPatterns, keepClause, whereClause);
  }

  public GraphPattern withWhereClause(hydra.util.Maybe<openGql.grammar.ValueExpression> whereClause) {
    return new GraphPattern(matchMode, pathPatterns, keepClause, whereClause);
  }
}
