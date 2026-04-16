// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ClosedEdgeReferenceValueType implements Serializable, Comparable<ClosedEdgeReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ClosedEdgeReferenceValueType");

  public static final hydra.core.Name EDGE_TYPE_SPEC = new hydra.core.Name("edgeTypeSpec");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final openGql.grammar.EdgeTypeSpecification edgeTypeSpec;

  public final Boolean notNull;

  public ClosedEdgeReferenceValueType (openGql.grammar.EdgeTypeSpecification edgeTypeSpec, Boolean notNull) {
    this.edgeTypeSpec = edgeTypeSpec;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClosedEdgeReferenceValueType)) {
      return false;
    }
    ClosedEdgeReferenceValueType o = (ClosedEdgeReferenceValueType) other;
    return java.util.Objects.equals(
      this.edgeTypeSpec,
      o.edgeTypeSpec) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(edgeTypeSpec) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClosedEdgeReferenceValueType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      edgeTypeSpec,
      other.edgeTypeSpec);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public ClosedEdgeReferenceValueType withEdgeTypeSpec(openGql.grammar.EdgeTypeSpecification edgeTypeSpec) {
    return new ClosedEdgeReferenceValueType(edgeTypeSpec, notNull);
  }

  public ClosedEdgeReferenceValueType withNotNull(Boolean notNull) {
    return new ClosedEdgeReferenceValueType(edgeTypeSpec, notNull);
  }
}
