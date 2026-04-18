// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A complete Coq .v file
 */
public class Document implements Serializable, Comparable<Document> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Document");

  public static final hydra.core.Name SENTENCES = new hydra.core.Name("sentences");

  public final java.util.List<hydra.coq.syntax.Sentence> sentences;

  public Document (java.util.List<hydra.coq.syntax.Sentence> sentences) {
    this.sentences = sentences;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Document)) {
      return false;
    }
    Document o = (Document) other;
    return java.util.Objects.equals(
      this.sentences,
      o.sentences);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(sentences);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Document other) {
    return hydra.util.Comparing.compare(
      sentences,
      other.sentences);
  }
}
