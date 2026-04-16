// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EdgeKeyLabelSetWithContent implements Serializable, Comparable<EdgeKeyLabelSetWithContent> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeKeyLabelSetWithContent");

  public static final hydra.core.Name KEY_LABEL_SET = new hydra.core.Name("keyLabelSet");

  public static final hydra.core.Name IMPLIED_CONTENT = new hydra.core.Name("impliedContent");

  public final hydra.util.Maybe<openGql.grammar.LabelSetPhrase> keyLabelSet;

  public final hydra.util.Maybe<openGql.grammar.EdgeTypeImpliedContent> impliedContent;

  public EdgeKeyLabelSetWithContent (hydra.util.Maybe<openGql.grammar.LabelSetPhrase> keyLabelSet, hydra.util.Maybe<openGql.grammar.EdgeTypeImpliedContent> impliedContent) {
    this.keyLabelSet = keyLabelSet;
    this.impliedContent = impliedContent;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeKeyLabelSetWithContent)) {
      return false;
    }
    EdgeKeyLabelSetWithContent o = (EdgeKeyLabelSetWithContent) other;
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
  public int compareTo(EdgeKeyLabelSetWithContent other) {
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

  public EdgeKeyLabelSetWithContent withKeyLabelSet(hydra.util.Maybe<openGql.grammar.LabelSetPhrase> keyLabelSet) {
    return new EdgeKeyLabelSetWithContent(keyLabelSet, impliedContent);
  }

  public EdgeKeyLabelSetWithContent withImpliedContent(hydra.util.Maybe<openGql.grammar.EdgeTypeImpliedContent> impliedContent) {
    return new EdgeKeyLabelSetWithContent(keyLabelSet, impliedContent);
  }
}
