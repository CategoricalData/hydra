// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NodeTypePattern implements Serializable, Comparable<NodeTypePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeTypePattern");

  public static final hydra.core.Name SYNONYM_AND_TYPE_NAME = new hydra.core.Name("synonymAndTypeName");

  public static final hydra.core.Name ALIAS = new hydra.core.Name("alias");

  public static final hydra.core.Name FILLER = new hydra.core.Name("filler");

  public final hydra.util.Maybe<openGql.grammar.NodeSynonymAndTypeName> synonymAndTypeName;

  public final hydra.util.Maybe<String> alias;

  public final hydra.util.Maybe<openGql.grammar.NodeTypeFiller> filler;

  public NodeTypePattern (hydra.util.Maybe<openGql.grammar.NodeSynonymAndTypeName> synonymAndTypeName, hydra.util.Maybe<String> alias, hydra.util.Maybe<openGql.grammar.NodeTypeFiller> filler) {
    this.synonymAndTypeName = synonymAndTypeName;
    this.alias = alias;
    this.filler = filler;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeTypePattern)) {
      return false;
    }
    NodeTypePattern o = (NodeTypePattern) other;
    return java.util.Objects.equals(
      this.synonymAndTypeName,
      o.synonymAndTypeName) && java.util.Objects.equals(
      this.alias,
      o.alias) && java.util.Objects.equals(
      this.filler,
      o.filler);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(synonymAndTypeName) + 3 * java.util.Objects.hashCode(alias) + 5 * java.util.Objects.hashCode(filler);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeTypePattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      synonymAndTypeName,
      other.synonymAndTypeName);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      alias,
      other.alias);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      filler,
      other.filler);
  }

  public NodeTypePattern withSynonymAndTypeName(hydra.util.Maybe<openGql.grammar.NodeSynonymAndTypeName> synonymAndTypeName) {
    return new NodeTypePattern(synonymAndTypeName, alias, filler);
  }

  public NodeTypePattern withAlias(hydra.util.Maybe<String> alias) {
    return new NodeTypePattern(synonymAndTypeName, alias, filler);
  }

  public NodeTypePattern withFiller(hydra.util.Maybe<openGql.grammar.NodeTypeFiller> filler) {
    return new NodeTypePattern(synonymAndTypeName, alias, filler);
  }
}
