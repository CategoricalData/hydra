// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A top-level sentence in a Coq document, optionally preceded by a comment
 */
public class Sentence implements Serializable, Comparable<Sentence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Sentence");

  public static final hydra.core.Name COMMENT = new hydra.core.Name("comment");

  public static final hydra.core.Name CONTENT = new hydra.core.Name("content");

  public final hydra.util.Maybe<hydra.coq.syntax.Comment> comment;

  public final hydra.coq.syntax.SentenceContent content;

  public Sentence (hydra.util.Maybe<hydra.coq.syntax.Comment> comment, hydra.coq.syntax.SentenceContent content) {
    this.comment = comment;
    this.content = content;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Sentence)) {
      return false;
    }
    Sentence o = (Sentence) other;
    return java.util.Objects.equals(
      this.comment,
      o.comment) && java.util.Objects.equals(
      this.content,
      o.content);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(comment) + 3 * java.util.Objects.hashCode(content);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Sentence other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      comment,
      other.comment);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      content,
      other.content);
  }

  public Sentence withComment(hydra.util.Maybe<hydra.coq.syntax.Comment> comment) {
    return new Sentence(comment, content);
  }

  public Sentence withContent(hydra.coq.syntax.SentenceContent content) {
    return new Sentence(comment, content);
  }
}
