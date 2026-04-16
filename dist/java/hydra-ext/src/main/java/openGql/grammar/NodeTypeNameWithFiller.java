// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NodeTypeNameWithFiller implements Serializable, Comparable<NodeTypeNameWithFiller> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeTypeNameWithFiller");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public static final hydra.core.Name FILLER = new hydra.core.Name("filler");

  public final String typeName;

  public final hydra.util.Maybe<openGql.grammar.NodeTypeFiller> filler;

  public NodeTypeNameWithFiller (String typeName, hydra.util.Maybe<openGql.grammar.NodeTypeFiller> filler) {
    this.typeName = typeName;
    this.filler = filler;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeTypeNameWithFiller)) {
      return false;
    }
    NodeTypeNameWithFiller o = (NodeTypeNameWithFiller) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.filler,
      o.filler);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(filler);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeTypeNameWithFiller other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typeName,
      other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      filler,
      other.filler);
  }

  public NodeTypeNameWithFiller withTypeName(String typeName) {
    return new NodeTypeNameWithFiller(typeName, filler);
  }

  public NodeTypeNameWithFiller withFiller(hydra.util.Maybe<openGql.grammar.NodeTypeFiller> filler) {
    return new NodeTypeNameWithFiller(typeName, filler);
  }
}
