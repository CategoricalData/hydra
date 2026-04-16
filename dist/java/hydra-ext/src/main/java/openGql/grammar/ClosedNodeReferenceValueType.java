// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ClosedNodeReferenceValueType implements Serializable, Comparable<ClosedNodeReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ClosedNodeReferenceValueType");

  public static final hydra.core.Name NODE_TYPE_SPEC = new hydra.core.Name("nodeTypeSpec");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final openGql.grammar.NodeTypeSpecification nodeTypeSpec;

  public final Boolean notNull;

  public ClosedNodeReferenceValueType (openGql.grammar.NodeTypeSpecification nodeTypeSpec, Boolean notNull) {
    this.nodeTypeSpec = nodeTypeSpec;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClosedNodeReferenceValueType)) {
      return false;
    }
    ClosedNodeReferenceValueType o = (ClosedNodeReferenceValueType) other;
    return java.util.Objects.equals(
      this.nodeTypeSpec,
      o.nodeTypeSpec) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(nodeTypeSpec) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClosedNodeReferenceValueType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      nodeTypeSpec,
      other.nodeTypeSpec);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public ClosedNodeReferenceValueType withNodeTypeSpec(openGql.grammar.NodeTypeSpecification nodeTypeSpec) {
    return new ClosedNodeReferenceValueType(nodeTypeSpec, notNull);
  }

  public ClosedNodeReferenceValueType withNotNull(Boolean notNull) {
    return new ClosedNodeReferenceValueType(nodeTypeSpec, notNull);
  }
}
