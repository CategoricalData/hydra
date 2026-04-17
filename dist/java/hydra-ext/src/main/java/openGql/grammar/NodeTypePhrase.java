// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NodeTypePhrase implements Serializable, Comparable<NodeTypePhrase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeTypePhrase");

  public static final hydra.core.Name SYNONYM = new hydra.core.Name("synonym");

  public static final hydra.core.Name TYPE_PHRASE_FILLER = new hydra.core.Name("typePhraseFiller");

  public static final hydra.core.Name ALIAS = new hydra.core.Name("alias");

  public final openGql.grammar.NodeSynonym synonym;

  public final openGql.grammar.NodeTypePhraseFiller typePhraseFiller;

  public final hydra.util.Maybe<String> alias;

  public NodeTypePhrase (openGql.grammar.NodeSynonym synonym, openGql.grammar.NodeTypePhraseFiller typePhraseFiller, hydra.util.Maybe<String> alias) {
    this.synonym = synonym;
    this.typePhraseFiller = typePhraseFiller;
    this.alias = alias;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeTypePhrase)) {
      return false;
    }
    NodeTypePhrase o = (NodeTypePhrase) other;
    return java.util.Objects.equals(
      this.synonym,
      o.synonym) && java.util.Objects.equals(
      this.typePhraseFiller,
      o.typePhraseFiller) && java.util.Objects.equals(
      this.alias,
      o.alias);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(synonym) + 3 * java.util.Objects.hashCode(typePhraseFiller) + 5 * java.util.Objects.hashCode(alias);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeTypePhrase other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      synonym,
      other.synonym);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      typePhraseFiller,
      other.typePhraseFiller);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      alias,
      other.alias);
  }

  public NodeTypePhrase withSynonym(openGql.grammar.NodeSynonym synonym) {
    return new NodeTypePhrase(synonym, typePhraseFiller, alias);
  }

  public NodeTypePhrase withTypePhraseFiller(openGql.grammar.NodeTypePhraseFiller typePhraseFiller) {
    return new NodeTypePhrase(synonym, typePhraseFiller, alias);
  }

  public NodeTypePhrase withAlias(hydra.util.Maybe<String> alias) {
    return new NodeTypePhrase(synonym, typePhraseFiller, alias);
  }
}
