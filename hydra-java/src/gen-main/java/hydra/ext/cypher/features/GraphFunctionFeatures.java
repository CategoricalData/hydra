// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Graph functions
 */
public class GraphFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.features.GraphFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH_BY_ELEMENT_ID = new hydra.core.Name("graph.byElementId");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH_BY_NAME = new hydra.core.Name("graph.byName");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH_NAMES = new hydra.core.Name("graph.names");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH_PROPERTIES_BY_NAME = new hydra.core.Name("graph.propertiesByName");
  
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
    java.util.Objects.requireNonNull((graph_byElementId));
    java.util.Objects.requireNonNull((graph_byName));
    java.util.Objects.requireNonNull((graph_names));
    java.util.Objects.requireNonNull((graph_propertiesByName));
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
    GraphFunctionFeatures o = (GraphFunctionFeatures) (other);
    return graph_byElementId.equals(o.graph_byElementId) && graph_byName.equals(o.graph_byName) && graph_names.equals(o.graph_names) && graph_propertiesByName.equals(o.graph_propertiesByName);
  }
  
  @Override
  public int hashCode() {
    return 2 * graph_byElementId.hashCode() + 3 * graph_byName.hashCode() + 5 * graph_names.hashCode() + 7 * graph_propertiesByName.hashCode();
  }
  
  public GraphFunctionFeatures withGraph_byElementId(Boolean graph_byElementId) {
    java.util.Objects.requireNonNull((graph_byElementId));
    return new GraphFunctionFeatures(graph_byElementId, graph_byName, graph_names, graph_propertiesByName);
  }
  
  public GraphFunctionFeatures withGraph_byName(Boolean graph_byName) {
    java.util.Objects.requireNonNull((graph_byName));
    return new GraphFunctionFeatures(graph_byElementId, graph_byName, graph_names, graph_propertiesByName);
  }
  
  public GraphFunctionFeatures withGraph_names(Boolean graph_names) {
    java.util.Objects.requireNonNull((graph_names));
    return new GraphFunctionFeatures(graph_byElementId, graph_byName, graph_names, graph_propertiesByName);
  }
  
  public GraphFunctionFeatures withGraph_propertiesByName(Boolean graph_propertiesByName) {
    java.util.Objects.requireNonNull((graph_propertiesByName));
    return new GraphFunctionFeatures(graph_byElementId, graph_byName, graph_names, graph_propertiesByName);
  }
}