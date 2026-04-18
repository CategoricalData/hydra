// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class GraphPatternBindingTable implements Serializable, Comparable<GraphPatternBindingTable> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphPatternBindingTable");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name YIELD_CLAUSE = new hydra.core.Name("yieldClause");

  public final openGql.grammar.GraphPattern pattern;

  public final hydra.util.Maybe<openGql.grammar.GraphPatternYieldItemList> yieldClause;

  public GraphPatternBindingTable (openGql.grammar.GraphPattern pattern, hydra.util.Maybe<openGql.grammar.GraphPatternYieldItemList> yieldClause) {
    this.pattern = pattern;
    this.yieldClause = yieldClause;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphPatternBindingTable)) {
      return false;
    }
    GraphPatternBindingTable o = (GraphPatternBindingTable) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.yieldClause,
      o.yieldClause);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(yieldClause);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GraphPatternBindingTable other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern,
      other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      yieldClause,
      other.yieldClause);
  }

  public GraphPatternBindingTable withPattern(openGql.grammar.GraphPattern pattern) {
    return new GraphPatternBindingTable(pattern, yieldClause);
  }

  public GraphPatternBindingTable withYieldClause(hydra.util.Maybe<openGql.grammar.GraphPatternYieldItemList> yieldClause) {
    return new GraphPatternBindingTable(pattern, yieldClause);
  }
}
