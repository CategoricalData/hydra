// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.features;

import java.io.Serializable;

/**
 * An interface that represents the capabilities of a Graph implementation. By default all methods of features return true and it is up to implementers to disable feature they don't support. Users should check features prior to using various functions of TinkerPop to help ensure code portability across implementations. For example, a common usage would be to check if a graph supports transactions prior to calling the commit method on Graph.tx().
 *
 * As an additional notice to Graph Providers, feature methods will be used by the test suite to determine which tests will be ignored and which will be executed, therefore proper setting of these features is essential to maximizing the amount of testing performed by the suite. Further note, that these methods may be called by the TinkerPop core code to determine what operations may be appropriately executed which will have impact on features utilized by users.
 */
public class Features implements Serializable, Comparable<Features> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.features.Features");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");

  /**
   * Gets the features related to edge operation.
   */
  public final hydra.tinkerpop.features.EdgeFeatures edge;

  /**
   * Gets the features related to graph operation.
   */
  public final hydra.tinkerpop.features.GraphFeatures graph;

  /**
   * Gets the features related to vertex operation.
   */
  public final hydra.tinkerpop.features.VertexFeatures vertex;

  public Features (hydra.tinkerpop.features.EdgeFeatures edge, hydra.tinkerpop.features.GraphFeatures graph, hydra.tinkerpop.features.VertexFeatures vertex) {
    this.edge = edge;
    this.graph = graph;
    this.vertex = vertex;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Features)) {
      return false;
    }
    Features o = (Features) other;
    return java.util.Objects.equals(
      this.edge,
      o.edge) && java.util.Objects.equals(
      this.graph,
      o.graph) && java.util.Objects.equals(
      this.vertex,
      o.vertex);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(edge) + 3 * java.util.Objects.hashCode(graph) + 5 * java.util.Objects.hashCode(vertex);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Features other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      edge,
      other.edge);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      graph,
      other.graph);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      vertex,
      other.vertex);
  }

  public Features withEdge(hydra.tinkerpop.features.EdgeFeatures edge) {
    return new Features(edge, graph, vertex);
  }

  public Features withGraph(hydra.tinkerpop.features.GraphFeatures graph) {
    return new Features(edge, graph, vertex);
  }

  public Features withVertex(hydra.tinkerpop.features.VertexFeatures vertex) {
    return new Features(edge, graph, vertex);
  }
}
