// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NodeSynonymAndTypeName implements Serializable, Comparable<NodeSynonymAndTypeName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeSynonymAndTypeName");

  public static final hydra.core.Name NODE_SYNONYM = new hydra.core.Name("nodeSynonym");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public final openGql.grammar.NodeSynonym nodeSynonym;

  public final hydra.util.Maybe<String> typeName;

  public NodeSynonymAndTypeName (openGql.grammar.NodeSynonym nodeSynonym, hydra.util.Maybe<String> typeName) {
    this.nodeSynonym = nodeSynonym;
    this.typeName = typeName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeSynonymAndTypeName)) {
      return false;
    }
    NodeSynonymAndTypeName o = (NodeSynonymAndTypeName) other;
    return java.util.Objects.equals(
      this.nodeSynonym,
      o.nodeSynonym) && java.util.Objects.equals(
      this.typeName,
      o.typeName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(nodeSynonym) + 3 * java.util.Objects.hashCode(typeName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeSynonymAndTypeName other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      nodeSynonym,
      other.nodeSynonym);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      typeName,
      other.typeName);
  }

  public NodeSynonymAndTypeName withNodeSynonym(openGql.grammar.NodeSynonym nodeSynonym) {
    return new NodeSynonymAndTypeName(nodeSynonym, typeName);
  }

  public NodeSynonymAndTypeName withTypeName(hydra.util.Maybe<String> typeName) {
    return new NodeSynonymAndTypeName(nodeSynonym, typeName);
  }
}
