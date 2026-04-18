// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class TypeDeclarationWithComments implements Serializable, Comparable<TypeDeclarationWithComments> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.TypeDeclarationWithComments");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name COMMENTS = new hydra.core.Name("comments");

  public final hydra.java.syntax.TypeDeclaration value;

  public final hydra.util.Maybe<String> comments;

  public TypeDeclarationWithComments (hydra.java.syntax.TypeDeclaration value, hydra.util.Maybe<String> comments) {
    this.value = value;
    this.comments = comments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeDeclarationWithComments)) {
      return false;
    }
    TypeDeclarationWithComments o = (TypeDeclarationWithComments) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.comments,
      o.comments);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(comments);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeDeclarationWithComments other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      comments,
      other.comments);
  }

  public TypeDeclarationWithComments withValue(hydra.java.syntax.TypeDeclaration value) {
    return new TypeDeclarationWithComments(value, comments);
  }

  public TypeDeclarationWithComments withComments(hydra.util.Maybe<String> comments) {
    return new TypeDeclarationWithComments(value, comments);
  }
}
