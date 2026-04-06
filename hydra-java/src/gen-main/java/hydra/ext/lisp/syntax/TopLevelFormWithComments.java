// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A top-level form together with optional documentation
 */
public class TopLevelFormWithComments implements Serializable, Comparable<TopLevelFormWithComments> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.TopLevelFormWithComments");

  public static final hydra.core.Name DOC = new hydra.core.Name("doc");

  public static final hydra.core.Name COMMENT = new hydra.core.Name("comment");

  public static final hydra.core.Name FORM = new hydra.core.Name("form");

  /**
   * Optional documentation string
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc;

  /**
   * Optional comment
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Comment> comment;

  /**
   * The form itself
   */
  public final hydra.ext.lisp.syntax.TopLevelForm form;

  public TopLevelFormWithComments (hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc, hydra.util.Maybe<hydra.ext.lisp.syntax.Comment> comment, hydra.ext.lisp.syntax.TopLevelForm form) {
    this.doc = doc;
    this.comment = comment;
    this.form = form;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TopLevelFormWithComments)) {
      return false;
    }
    TopLevelFormWithComments o = (TopLevelFormWithComments) other;
    return java.util.Objects.equals(
      this.doc,
      o.doc) && java.util.Objects.equals(
      this.comment,
      o.comment) && java.util.Objects.equals(
      this.form,
      o.form);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(doc) + 3 * java.util.Objects.hashCode(comment) + 5 * java.util.Objects.hashCode(form);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TopLevelFormWithComments other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      doc,
      other.doc);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      comment,
      other.comment);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      form,
      other.form);
  }

  public TopLevelFormWithComments withDoc(hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc) {
    return new TopLevelFormWithComments(doc, comment, form);
  }

  public TopLevelFormWithComments withComment(hydra.util.Maybe<hydra.ext.lisp.syntax.Comment> comment) {
    return new TopLevelFormWithComments(doc, comment, form);
  }

  public TopLevelFormWithComments withForm(hydra.ext.lisp.syntax.TopLevelForm form) {
    return new TopLevelFormWithComments(doc, comment, form);
  }
}
