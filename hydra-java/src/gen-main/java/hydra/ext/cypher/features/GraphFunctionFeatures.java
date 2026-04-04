// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Graph functions
 */
public class GraphFunctionFeatures implements Serializable, Comparable<GraphFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.GraphFunctionFeatures");

  public static final hydra.core.Name GRAPH_BY_ELEMENT_ID = new hydra.core.Name("graphByElementId");

  public static final hydra.core.Name GRAPH_BY_NAME = new hydra.core.Name("graphByName");

  public static final hydra.core.Name GRAPH_NAMES = new hydra.core.Name("graphNames");

  public static final hydra.core.Name GRAPH_PROPERTIES_BY_NAME = new hydra.core.Name("graphPropertiesByName");

  /**
   * The graph.byElementId() function. Resolves the constituent graph to which a given element id belongs. Introduced in 5.13.
   */
  public final Boolean graphByElementId;

  /**
   * The graph.byName() function. Resolves a constituent graph by name.
   */
  public final Boolean graphByName;

  /**
   * The graph.names() function. Returns a list containing the names of all graphs in the current composite database.
   */
  public final Boolean graphNames;

  /**
   * The graph.propertiesByName() function. Returns a map containing the properties associated with the given graph.
   */
  public final Boolean graphPropertiesByName;

  public GraphFunctionFeatures (Boolean graphByElementId, Boolean graphByName, Boolean graphNames, Boolean graphPropertiesByName) {
    this.graphByElementId = graphByElementId;
    this.graphByName = graphByName;
    this.graphNames = graphNames;
    this.graphPropertiesByName = graphPropertiesByName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphFunctionFeatures)) {
      return false;
    }
    GraphFunctionFeatures o = (GraphFunctionFeatures) other;
    return java.util.Objects.equals(
      this.graphByElementId,
      o.graphByElementId) && java.util.Objects.equals(
      this.graphByName,
      o.graphByName) && java.util.Objects.equals(
      this.graphNames,
      o.graphNames) && java.util.Objects.equals(
      this.graphPropertiesByName,
      o.graphPropertiesByName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(graphByElementId) + 3 * java.util.Objects.hashCode(graphByName) + 5 * java.util.Objects.hashCode(graphNames) + 7 * java.util.Objects.hashCode(graphPropertiesByName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GraphFunctionFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      graphByElementId,
      other.graphByElementId);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      graphByName,
      other.graphByName);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      graphNames,
      other.graphNames);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      graphPropertiesByName,
      other.graphPropertiesByName);
  }

  public GraphFunctionFeatures withGraphByElementId(Boolean graphByElementId) {
    return new GraphFunctionFeatures(graphByElementId, graphByName, graphNames, graphPropertiesByName);
  }

  public GraphFunctionFeatures withGraphByName(Boolean graphByName) {
    return new GraphFunctionFeatures(graphByElementId, graphByName, graphNames, graphPropertiesByName);
  }

  public GraphFunctionFeatures withGraphNames(Boolean graphNames) {
    return new GraphFunctionFeatures(graphByElementId, graphByName, graphNames, graphPropertiesByName);
  }

  public GraphFunctionFeatures withGraphPropertiesByName(Boolean graphPropertiesByName) {
    return new GraphFunctionFeatures(graphByElementId, graphByName, graphNames, graphPropertiesByName);
  }
}
