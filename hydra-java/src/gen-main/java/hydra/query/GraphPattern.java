// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A query pattern which matches within a designated component subgraph
 */
public class GraphPattern implements Serializable, Comparable<GraphPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.query.GraphPattern");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name PATTERNS = new hydra.core.Name("patterns");

  /**
   * The name of the component graph
   */
  public final hydra.core.Name graph;

  /**
   * The patterns to match within the subgraph
   */
  public final java.util.List<hydra.query.Pattern> patterns;

  public GraphPattern (hydra.core.Name graph, java.util.List<hydra.query.Pattern> patterns) {
    this.graph = graph;
    this.patterns = patterns;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphPattern)) {
      return false;
    }
    GraphPattern o = (GraphPattern) other;
    return java.util.Objects.equals(
      this.graph,
      o.graph) && java.util.Objects.equals(
      this.patterns,
      o.patterns);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(graph) + 3 * java.util.Objects.hashCode(patterns);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GraphPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      graph,
      other.graph);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      patterns,
      other.patterns);
  }

  public GraphPattern withGraph(hydra.core.Name graph) {
    return new GraphPattern(graph, patterns);
  }

  public GraphPattern withPatterns(java.util.List<hydra.query.Pattern> patterns) {
    return new GraphPattern(graph, patterns);
  }
}
