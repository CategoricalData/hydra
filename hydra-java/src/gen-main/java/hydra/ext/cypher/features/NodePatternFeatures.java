// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Node patterns
 */
public class NodePatternFeatures implements Serializable, Comparable<NodePatternFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.NodePatternFeatures");

  public static final hydra.core.Name MULTIPLE_LABELS = new hydra.core.Name("multipleLabels");

  public static final hydra.core.Name PARAMETER = new hydra.core.Name("parameter");

  public static final hydra.core.Name PROPERTY_MAP = new hydra.core.Name("propertyMap");

  public static final hydra.core.Name VARIABLE_NODE = new hydra.core.Name("variableNode");

  public static final hydra.core.Name WILDCARD_LABEL = new hydra.core.Name("wildcardLabel");

  /**
   * Specifying multiple labels in a node pattern
   */
  public final Boolean multipleLabels;

  /**
   * Specifying a parameter as part of a node pattern
   */
  public final Boolean parameter;

  /**
   * Specifying a key/value map of properties in a node pattern
   */
  public final Boolean propertyMap;

  /**
   * Binding a variable to a node in a node pattern (note: included by most if not all implementations).
   */
  public final Boolean variableNode;

  /**
   * Omitting labels from a node pattern
   */
  public final Boolean wildcardLabel;

  public NodePatternFeatures (Boolean multipleLabels, Boolean parameter, Boolean propertyMap, Boolean variableNode, Boolean wildcardLabel) {
    this.multipleLabels = multipleLabels;
    this.parameter = parameter;
    this.propertyMap = propertyMap;
    this.variableNode = variableNode;
    this.wildcardLabel = wildcardLabel;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodePatternFeatures)) {
      return false;
    }
    NodePatternFeatures o = (NodePatternFeatures) other;
    return java.util.Objects.equals(
      this.multipleLabels,
      o.multipleLabels) && java.util.Objects.equals(
      this.parameter,
      o.parameter) && java.util.Objects.equals(
      this.propertyMap,
      o.propertyMap) && java.util.Objects.equals(
      this.variableNode,
      o.variableNode) && java.util.Objects.equals(
      this.wildcardLabel,
      o.wildcardLabel);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(multipleLabels) + 3 * java.util.Objects.hashCode(parameter) + 5 * java.util.Objects.hashCode(propertyMap) + 7 * java.util.Objects.hashCode(variableNode) + 11 * java.util.Objects.hashCode(wildcardLabel);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodePatternFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) multipleLabels).compareTo(other.multipleLabels);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) parameter).compareTo(other.parameter);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) propertyMap).compareTo(other.propertyMap);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) variableNode).compareTo(other.variableNode);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) wildcardLabel).compareTo(other.wildcardLabel);
  }

  public NodePatternFeatures withMultipleLabels(Boolean multipleLabels) {
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }

  public NodePatternFeatures withParameter(Boolean parameter) {
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }

  public NodePatternFeatures withPropertyMap(Boolean propertyMap) {
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }

  public NodePatternFeatures withVariableNode(Boolean variableNode) {
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }

  public NodePatternFeatures withWildcardLabel(Boolean wildcardLabel) {
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
}
