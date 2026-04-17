// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SelectGraphMatch implements Serializable, Comparable<SelectGraphMatch> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SelectGraphMatch");

  public static final hydra.core.Name GRAPH_EXPRESSION = new hydra.core.Name("graphExpression");

  public static final hydra.core.Name MATCH_STATEMENT = new hydra.core.Name("matchStatement");

  public final openGql.grammar.GraphExpression graphExpression;

  public final openGql.grammar.MatchStatement matchStatement;

  public SelectGraphMatch (openGql.grammar.GraphExpression graphExpression, openGql.grammar.MatchStatement matchStatement) {
    this.graphExpression = graphExpression;
    this.matchStatement = matchStatement;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelectGraphMatch)) {
      return false;
    }
    SelectGraphMatch o = (SelectGraphMatch) other;
    return java.util.Objects.equals(
      this.graphExpression,
      o.graphExpression) && java.util.Objects.equals(
      this.matchStatement,
      o.matchStatement);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(graphExpression) + 3 * java.util.Objects.hashCode(matchStatement);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SelectGraphMatch other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      graphExpression,
      other.graphExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      matchStatement,
      other.matchStatement);
  }

  public SelectGraphMatch withGraphExpression(openGql.grammar.GraphExpression graphExpression) {
    return new SelectGraphMatch(graphExpression, matchStatement);
  }

  public SelectGraphMatch withMatchStatement(openGql.grammar.MatchStatement matchStatement) {
    return new SelectGraphMatch(graphExpression, matchStatement);
  }
}
