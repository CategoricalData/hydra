// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NodeKeyLabelSetWithContent implements Serializable, Comparable<NodeKeyLabelSetWithContent> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeKeyLabelSetWithContent");

  public static final hydra.core.Name KEY_LABEL_SET = new hydra.core.Name("keyLabelSet");

  public static final hydra.core.Name IMPLIED_CONTENT = new hydra.core.Name("impliedContent");

  public final hydra.util.Maybe<openGql.grammar.LabelSetPhrase> keyLabelSet;

  public final hydra.util.Maybe<openGql.grammar.NodeTypeImpliedContent> impliedContent;

  public NodeKeyLabelSetWithContent (hydra.util.Maybe<openGql.grammar.LabelSetPhrase> keyLabelSet, hydra.util.Maybe<openGql.grammar.NodeTypeImpliedContent> impliedContent) {
    this.keyLabelSet = keyLabelSet;
    this.impliedContent = impliedContent;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeKeyLabelSetWithContent)) {
      return false;
    }
    NodeKeyLabelSetWithContent o = (NodeKeyLabelSetWithContent) other;
    return java.util.Objects.equals(
      this.keyLabelSet,
      o.keyLabelSet) && java.util.Objects.equals(
      this.impliedContent,
      o.impliedContent);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(keyLabelSet) + 3 * java.util.Objects.hashCode(impliedContent);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeKeyLabelSetWithContent other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      keyLabelSet,
      other.keyLabelSet);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      impliedContent,
      other.impliedContent);
  }

  public NodeKeyLabelSetWithContent withKeyLabelSet(hydra.util.Maybe<openGql.grammar.LabelSetPhrase> keyLabelSet) {
    return new NodeKeyLabelSetWithContent(keyLabelSet, impliedContent);
  }

  public NodeKeyLabelSetWithContent withImpliedContent(hydra.util.Maybe<openGql.grammar.NodeTypeImpliedContent> impliedContent) {
    return new NodeKeyLabelSetWithContent(keyLabelSet, impliedContent);
  }
}
