// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A Section ... End block
 */
public class SectionDefinition implements Serializable, Comparable<SectionDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.SectionDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name SENTENCES = new hydra.core.Name("sentences");

  public final hydra.coq.syntax.Ident name;

  public final java.util.List<hydra.coq.syntax.Sentence> sentences;

  public SectionDefinition (hydra.coq.syntax.Ident name, java.util.List<hydra.coq.syntax.Sentence> sentences) {
    this.name = name;
    this.sentences = sentences;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SectionDefinition)) {
      return false;
    }
    SectionDefinition o = (SectionDefinition) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.sentences,
      o.sentences);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(sentences);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SectionDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sentences,
      other.sentences);
  }

  public SectionDefinition withName(hydra.coq.syntax.Ident name) {
    return new SectionDefinition(name, sentences);
  }

  public SectionDefinition withSentences(java.util.List<hydra.coq.syntax.Sentence> sentences) {
    return new SectionDefinition(name, sentences);
  }
}
