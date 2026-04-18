// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NodeLabelSetWithProperties implements Serializable, Comparable<NodeLabelSetWithProperties> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeLabelSetWithProperties");

  public static final hydra.core.Name LABEL_SET = new hydra.core.Name("labelSet");

  public static final hydra.core.Name PROPERTY_TYPES = new hydra.core.Name("propertyTypes");

  public final openGql.grammar.LabelSetPhrase labelSet;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.PropertyType>> propertyTypes;

  public NodeLabelSetWithProperties (openGql.grammar.LabelSetPhrase labelSet, hydra.util.Maybe<java.util.List<openGql.grammar.PropertyType>> propertyTypes) {
    this.labelSet = labelSet;
    this.propertyTypes = propertyTypes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeLabelSetWithProperties)) {
      return false;
    }
    NodeLabelSetWithProperties o = (NodeLabelSetWithProperties) other;
    return java.util.Objects.equals(
      this.labelSet,
      o.labelSet) && java.util.Objects.equals(
      this.propertyTypes,
      o.propertyTypes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(labelSet) + 3 * java.util.Objects.hashCode(propertyTypes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeLabelSetWithProperties other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      labelSet,
      other.labelSet);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      propertyTypes,
      other.propertyTypes);
  }

  public NodeLabelSetWithProperties withLabelSet(openGql.grammar.LabelSetPhrase labelSet) {
    return new NodeLabelSetWithProperties(labelSet, propertyTypes);
  }

  public NodeLabelSetWithProperties withPropertyTypes(hydra.util.Maybe<java.util.List<openGql.grammar.PropertyType>> propertyTypes) {
    return new NodeLabelSetWithProperties(labelSet, propertyTypes);
  }
}
