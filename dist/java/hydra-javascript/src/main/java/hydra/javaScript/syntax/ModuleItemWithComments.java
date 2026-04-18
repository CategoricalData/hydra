// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A module item with optional documentation
 */
public class ModuleItemWithComments implements Serializable, Comparable<ModuleItemWithComments> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ModuleItemWithComments");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name COMMENTS = new hydra.core.Name("comments");

  /**
   * The module item
   */
  public final hydra.javaScript.syntax.ModuleItem body;

  /**
   * Optional documentation comment
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments;

  public ModuleItemWithComments (hydra.javaScript.syntax.ModuleItem body, hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments) {
    this.body = body;
    this.comments = comments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleItemWithComments)) {
      return false;
    }
    ModuleItemWithComments o = (ModuleItemWithComments) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.comments,
      o.comments);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(comments);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleItemWithComments other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      comments,
      other.comments);
  }

  public ModuleItemWithComments withBody(hydra.javaScript.syntax.ModuleItem body) {
    return new ModuleItemWithComments(body, comments);
  }

  public ModuleItemWithComments withComments(hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments) {
    return new ModuleItemWithComments(body, comments);
  }
}
