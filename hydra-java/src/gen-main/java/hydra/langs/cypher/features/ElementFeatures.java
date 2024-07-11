// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for element functions.
 */
public class ElementFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.ElementFeatures");
  
  /**
   * Whether to expect the elementId() function.
   */
  public final Boolean elementId;
  
  /**
   * Whether to expect the endNode() function.
   */
  public final Boolean endNode;
  
  /**
   * Whether to expect the labels() function.
   */
  public final Boolean labels;
  
  /**
   * Whether to expect the properties() function.
   */
  public final Boolean properties;
  
  /**
   * Whether to expect the startNode() function.
   */
  public final Boolean startNode;
  
  public ElementFeatures (Boolean elementId, Boolean endNode, Boolean labels, Boolean properties, Boolean startNode) {
    java.util.Objects.requireNonNull((elementId));
    java.util.Objects.requireNonNull((endNode));
    java.util.Objects.requireNonNull((labels));
    java.util.Objects.requireNonNull((properties));
    java.util.Objects.requireNonNull((startNode));
    this.elementId = elementId;
    this.endNode = endNode;
    this.labels = labels;
    this.properties = properties;
    this.startNode = startNode;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementFeatures)) {
      return false;
    }
    ElementFeatures o = (ElementFeatures) (other);
    return elementId.equals(o.elementId) && endNode.equals(o.endNode) && labels.equals(o.labels) && properties.equals(o.properties) && startNode.equals(o.startNode);
  }
  
  @Override
  public int hashCode() {
    return 2 * elementId.hashCode() + 3 * endNode.hashCode() + 5 * labels.hashCode() + 7 * properties.hashCode() + 11 * startNode.hashCode();
  }
  
  public ElementFeatures withElementId(Boolean elementId) {
    java.util.Objects.requireNonNull((elementId));
    return new ElementFeatures(elementId, endNode, labels, properties, startNode);
  }
  
  public ElementFeatures withEndNode(Boolean endNode) {
    java.util.Objects.requireNonNull((endNode));
    return new ElementFeatures(elementId, endNode, labels, properties, startNode);
  }
  
  public ElementFeatures withLabels(Boolean labels) {
    java.util.Objects.requireNonNull((labels));
    return new ElementFeatures(elementId, endNode, labels, properties, startNode);
  }
  
  public ElementFeatures withProperties(Boolean properties) {
    java.util.Objects.requireNonNull((properties));
    return new ElementFeatures(elementId, endNode, labels, properties, startNode);
  }
  
  public ElementFeatures withStartNode(Boolean startNode) {
    java.util.Objects.requireNonNull((startNode));
    return new ElementFeatures(elementId, endNode, labels, properties, startNode);
  }
}