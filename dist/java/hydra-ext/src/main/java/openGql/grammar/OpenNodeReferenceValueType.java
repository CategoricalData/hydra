// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OpenNodeReferenceValueType implements Serializable, Comparable<OpenNodeReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OpenNodeReferenceValueType");

  public static final hydra.core.Name ANY = new hydra.core.Name("any");

  public static final hydra.core.Name NODE_SYNONYM = new hydra.core.Name("nodeSynonym");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean any;

  public final openGql.grammar.NodeSynonym nodeSynonym;

  public final Boolean notNull;

  public OpenNodeReferenceValueType (Boolean any, openGql.grammar.NodeSynonym nodeSynonym, Boolean notNull) {
    this.any = any;
    this.nodeSynonym = nodeSynonym;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpenNodeReferenceValueType)) {
      return false;
    }
    OpenNodeReferenceValueType o = (OpenNodeReferenceValueType) other;
    return java.util.Objects.equals(
      this.any,
      o.any) && java.util.Objects.equals(
      this.nodeSynonym,
      o.nodeSynonym) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(any) + 3 * java.util.Objects.hashCode(nodeSynonym) + 5 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OpenNodeReferenceValueType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      any,
      other.any);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      nodeSynonym,
      other.nodeSynonym);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public OpenNodeReferenceValueType withAny(Boolean any) {
    return new OpenNodeReferenceValueType(any, nodeSynonym, notNull);
  }

  public OpenNodeReferenceValueType withNodeSynonym(openGql.grammar.NodeSynonym nodeSynonym) {
    return new OpenNodeReferenceValueType(any, nodeSynonym, notNull);
  }

  public OpenNodeReferenceValueType withNotNull(Boolean notNull) {
    return new OpenNodeReferenceValueType(any, nodeSynonym, notNull);
  }
}
