// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A documentation tag (@param, @returns, @type, etc.)
 */
public class DocumentationTag implements Serializable, Comparable<DocumentationTag> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.DocumentationTag");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name PARAM_NAME = new hydra.core.Name("paramName");

  public static final hydra.core.Name DESCRIPTION = new hydra.core.Name("description");

  /**
   * Tag name (param, returns, type, etc.)
   */
  public final String name;

  /**
   * Optional type expression
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> type;

  /**
   * Optional parameter name (for @param)
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.Identifier> paramName;

  /**
   * Tag description
   */
  public final String description;

  public DocumentationTag (String name, hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> type, hydra.util.Maybe<hydra.javaScript.syntax.Identifier> paramName, String description) {
    this.name = name;
    this.type = type;
    this.paramName = paramName;
    this.description = description;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DocumentationTag)) {
      return false;
    }
    DocumentationTag o = (DocumentationTag) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.paramName,
      o.paramName) && java.util.Objects.equals(
      this.description,
      o.description);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(paramName) + 7 * java.util.Objects.hashCode(description);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DocumentationTag other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      paramName,
      other.paramName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      description,
      other.description);
  }

  public DocumentationTag withName(String name) {
    return new DocumentationTag(name, type, paramName, description);
  }

  public DocumentationTag withType(hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> type) {
    return new DocumentationTag(name, type, paramName, description);
  }

  public DocumentationTag withParamName(hydra.util.Maybe<hydra.javaScript.syntax.Identifier> paramName) {
    return new DocumentationTag(name, type, paramName, description);
  }

  public DocumentationTag withDescription(String description) {
    return new DocumentationTag(name, type, paramName, description);
  }
}
