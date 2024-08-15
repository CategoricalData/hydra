// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for node patterns.
 */
public class NodePatternFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.NodePatternFeatures");
  
  public static final hydra.core.Name FIELD_NAME_MULTIPLE_LABELS = new hydra.core.Name("multipleLabels");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_MAP = new hydra.core.Name("propertyMap");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE_NODE = new hydra.core.Name("variableNode");
  
  public static final hydra.core.Name FIELD_NAME_WILDCARD_LABEL = new hydra.core.Name("wildcardLabel");
  
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
    java.util.Objects.requireNonNull((multipleLabels));
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((propertyMap));
    java.util.Objects.requireNonNull((variableNode));
    java.util.Objects.requireNonNull((wildcardLabel));
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
    java.util.Objects.requireNonNull((multipleLabels));
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
  
  public NodePatternFeatures withParameter(Boolean parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
  
  public NodePatternFeatures withPropertyMap(Boolean propertyMap) {
    java.util.Objects.requireNonNull((propertyMap));
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
  
  public NodePatternFeatures withVariableNode(Boolean variableNode) {
    java.util.Objects.requireNonNull((variableNode));
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
  
  public NodePatternFeatures withWildcardLabel(Boolean wildcardLabel) {
    java.util.Objects.requireNonNull((wildcardLabel));
    return new NodePatternFeatures(multipleLabels, parameter, propertyMap, variableNode, wildcardLabel);
  }
}