// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A documentation comment (JSDoc) with structured tags
 */
public class DocumentationComment implements Serializable, Comparable<DocumentationComment> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.DocumentationComment");

  public static final hydra.core.Name DESCRIPTION = new hydra.core.Name("description");

  public static final hydra.core.Name TAGS = new hydra.core.Name("tags");

  /**
   * The main description
   */
  public final String description;

  /**
   * Documentation tags (@param, @returns, etc.)
   */
  public final java.util.List<hydra.javaScript.syntax.DocumentationTag> tags;

  public DocumentationComment (String description, java.util.List<hydra.javaScript.syntax.DocumentationTag> tags) {
    this.description = description;
    this.tags = tags;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DocumentationComment)) {
      return false;
    }
    DocumentationComment o = (DocumentationComment) other;
    return java.util.Objects.equals(
      this.description,
      o.description) && java.util.Objects.equals(
      this.tags,
      o.tags);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(description) + 3 * java.util.Objects.hashCode(tags);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DocumentationComment other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      description,
      other.description);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      tags,
      other.tags);
  }

  public DocumentationComment withDescription(String description) {
    return new DocumentationComment(description, tags);
  }

  public DocumentationComment withTags(java.util.List<hydra.javaScript.syntax.DocumentationTag> tags) {
    return new DocumentationComment(description, tags);
  }
}
