// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OpenEdgeReferenceValueType implements Serializable, Comparable<OpenEdgeReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OpenEdgeReferenceValueType");

  public static final hydra.core.Name ANY = new hydra.core.Name("any");

  public static final hydra.core.Name EDGE_SYNONYM = new hydra.core.Name("edgeSynonym");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean any;

  public final openGql.grammar.EdgeSynonym edgeSynonym;

  public final Boolean notNull;

  public OpenEdgeReferenceValueType (Boolean any, openGql.grammar.EdgeSynonym edgeSynonym, Boolean notNull) {
    this.any = any;
    this.edgeSynonym = edgeSynonym;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpenEdgeReferenceValueType)) {
      return false;
    }
    OpenEdgeReferenceValueType o = (OpenEdgeReferenceValueType) other;
    return java.util.Objects.equals(
      this.any,
      o.any) && java.util.Objects.equals(
      this.edgeSynonym,
      o.edgeSynonym) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(any) + 3 * java.util.Objects.hashCode(edgeSynonym) + 5 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OpenEdgeReferenceValueType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      any,
      other.any);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      edgeSynonym,
      other.edgeSynonym);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public OpenEdgeReferenceValueType withAny(Boolean any) {
    return new OpenEdgeReferenceValueType(any, edgeSynonym, notNull);
  }

  public OpenEdgeReferenceValueType withEdgeSynonym(openGql.grammar.EdgeSynonym edgeSynonym) {
    return new OpenEdgeReferenceValueType(any, edgeSynonym, notNull);
  }

  public OpenEdgeReferenceValueType withNotNull(Boolean notNull) {
    return new OpenEdgeReferenceValueType(any, edgeSynonym, notNull);
  }
}
