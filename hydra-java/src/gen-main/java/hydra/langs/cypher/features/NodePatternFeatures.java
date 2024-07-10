// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for node patterns.
 */
public class NodePatternFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.NodePatternFeatures");
  
  /**
   * Whether to expect specifying multiple labels in a node pattern.
   */
  public final Boolean multipleLabels;
  
  /**
   * Whether to expect specifying a parameter as part of a node pattern.
   */
  public final Boolean parameter;
  
  /**
   * Whether to expect specifying a key/value map of properties in a node pattern.
   */
  public final Boolean propertyMap;
  
  /**
   * Whether to expect binding a variable to a node in a node pattern (note: included by most if not all implementations).
   */
  public final Boolean variableNode;
  
  /**
   * Whether to expect omitting labels from a node pattern.
   */
  public final Boolean wildcardLabel;
  
  public NodePatternFeatures (Boolean multipleLabels, Boolean parameter, Boolean propertyMap, Boolean variableNode, Boolean wildcardLabel) {
    if (multipleLabels == null) {
      throw new IllegalArgumentException("null value for 'multipleLabels' argument");
    }
    if (parameter == null) {
      throw new IllegalArgumentException("null value for 'parameter' argument");
    }
    if (propertyMap == null) {
      throw new IllegalArgumentException("null value for 'propertyMap' argument");
    }
    if (variableNode == null) {
      throw new IllegalArgumentException("null value for 'variableNode' argument");
    }
    if (wildcardLabel == null) {
      throw new IllegalArgumentException("null value for 'wildcardLabel' argument");
    }
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
    NodePatternFeatures o = (NodePatternFeatures) (other);
    return multipleLabels.equals(o.multipleLabels) && parameter.equals(o.parameter) && propertyMap.equals(o.propertyMap) && variableNode.equals(o.variableNode) && wildcardLabel.equals(o.wildcardLabel);
  }
  
  @Override
  public int hashCode() {
    return 2 * multipleLabels.hashCode() + 3 * parameter.hashCode() + 5 * propertyMap.hashCode() + 7 * variableNode.hashCode() + 11 * wildcardLabel.hashCode();
  }
  
  public NodePatternFeatures withMultipleLabels(Boolean multipleLabels) {
    if (multipleLabels == null) {
      throw new IllegalArgumentException("null value for 'multipleLabels' argument");
    }
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
  
  public NodePatternFeatures withParameter(Boolean parameter) {
    if (parameter == null) {
      throw new IllegalArgumentException("null value for 'parameter' argument");
    }
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
  
  public NodePatternFeatures withPropertyMap(Boolean propertyMap) {
    if (propertyMap == null) {
      throw new IllegalArgumentException("null value for 'propertyMap' argument");
    }
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
  
  public NodePatternFeatures withVariableNode(Boolean variableNode) {
    if (variableNode == null) {
      throw new IllegalArgumentException("null value for 'variableNode' argument");
    }
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
  
  public NodePatternFeatures withWildcardLabel(Boolean wildcardLabel) {
    if (wildcardLabel == null) {
      throw new IllegalArgumentException("null value for 'wildcardLabel' argument");
    }
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
}