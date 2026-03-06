// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Graph functions
 */
public class GraphFunctionFeatures implements Serializable, Comparable<GraphFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.GraphFunctionFeatures");
  
  public static final hydra.core.Name GRAPH_BY_ELEMENT_ID = new hydra.core.Name("graph.byElementId");
  
  public static final hydra.core.Name GRAPH_BY_NAME = new hydra.core.Name("graph.byName");
  
  public static final hydra.core.Name GRAPH_NAMES = new hydra.core.Name("graph.names");
  
  public static final hydra.core.Name GRAPH_PROPERTIES_BY_NAME = new hydra.core.Name("graph.propertiesByName");
  
  /**
   * The graph.byElementId() function. Resolves the constituent graph to which a given element id belongs. Introduced in 5.13.
   */
  public final Boolean graph_byElementId;
  
  /**
   * The graph.byName() function. Resolves a constituent graph by name.
   */
  public final Boolean graph_byName;
  
  /**
   * The graph.names() function. Returns a list containing the names of all graphs in the current composite database.
   */
  public final Boolean graph_names;
  
  /**
   * The graph.propertiesByName() function. Returns a map containing the properties associated with the given graph.
   */
  public final Boolean graph_propertiesByName;
  
  public GraphFunctionFeatures (Boolean graph_byElementId, Boolean graph_byName, Boolean graph_names, Boolean graph_propertiesByName) {
    this.graph_byElementId = graph_byElementId;
    this.graph_byName = graph_byName;
    this.graph_names = graph_names;
    this.graph_propertiesByName = graph_propertiesByName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphFunctionFeatures)) {
      return false;
    }
    GraphFunctionFeatures o = (GraphFunctionFeatures) other;
    return java.util.Objects.equals(
      this.graph_byElementId,
      o.graph_byElementId) && java.util.Objects.equals(
      this.graph_byName,
      o.graph_byName) && java.util.Objects.equals(
      this.graph_names,
      o.graph_names) && java.util.Objects.equals(
      this.graph_propertiesByName,
      o.graph_propertiesByName);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(graph_byElementId) + 3 * java.util.Objects.hashCode(graph_byName) + 5 * java.util.Objects.hashCode(graph_names) + 7 * java.util.Objects.hashCode(graph_propertiesByName);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GraphFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) graph_byElementId).compareTo(other.graph_byElementId);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) graph_byName).compareTo(other.graph_byName);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) graph_names).compareTo(other.graph_names);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) graph_propertiesByName).compareTo(other.graph_propertiesByName);
  }
  
  public GraphFunctionFeatures withGraph_byElementId(Boolean graph_byElementId) {
    return new GraphFunctionFeatures(graph_byElementId, graph_byName, graph_names, graph_propertiesByName);
  }
  
  public GraphFunctionFeatures withGraph_byName(Boolean graph_byName) {
    return new GraphFunctionFeatures(graph_byElementId, graph_byName, graph_names, graph_propertiesByName);
  }
  
  public GraphFunctionFeatures withGraph_names(Boolean graph_names) {
    return new GraphFunctionFeatures(graph_byElementId, graph_byName, graph_names, graph_propertiesByName);
  }
  
  public GraphFunctionFeatures withGraph_propertiesByName(Boolean graph_propertiesByName) {
    return new GraphFunctionFeatures(graph_byElementId, graph_byName, graph_names, graph_propertiesByName);
  }
}
