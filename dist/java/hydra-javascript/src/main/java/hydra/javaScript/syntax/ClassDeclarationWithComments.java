// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A class declaration with optional JSDoc
 */
public class ClassDeclarationWithComments implements Serializable, Comparable<ClassDeclarationWithComments> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ClassDeclarationWithComments");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name COMMENTS = new hydra.core.Name("comments");

  /**
   * The class declaration
   */
  public final hydra.javaScript.syntax.ClassDeclaration body;

  /**
   * Optional JSDoc comment
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments;

  public ClassDeclarationWithComments (hydra.javaScript.syntax.ClassDeclaration body, hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments) {
    this.body = body;
    this.comments = comments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassDeclarationWithComments)) {
      return false;
    }
    ClassDeclarationWithComments o = (ClassDeclarationWithComments) other;
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
  public int compareTo(ClassDeclarationWithComments other) {
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

  public ClassDeclarationWithComments withBody(hydra.javaScript.syntax.ClassDeclaration body) {
    return new ClassDeclarationWithComments(body, comments);
  }

  public ClassDeclarationWithComments withComments(hydra.util.Maybe<hydra.javaScript.syntax.DocumentationComment> comments) {
    return new ClassDeclarationWithComments(body, comments);
  }
}
