// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ClassBodyDeclarationWithComments implements Serializable, Comparable<ClassBodyDeclarationWithComments> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ClassBodyDeclarationWithComments");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_COMMENTS = new hydra.core.Name("comments");
  
  public final hydra.ext.java.syntax.ClassBodyDeclaration value;
  
  public final hydra.util.Maybe<String> comments;
  
  public ClassBodyDeclarationWithComments (hydra.ext.java.syntax.ClassBodyDeclaration value, hydra.util.Maybe<String> comments) {
    this.value = value;
    this.comments = comments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassBodyDeclarationWithComments)) {
      return false;
    }
    ClassBodyDeclarationWithComments o = (ClassBodyDeclarationWithComments) other;
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
  public int compareTo(ClassBodyDeclarationWithComments other) {
    int cmp = 0;
    cmp = ((Comparable) value).compareTo(other.value);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      comments.hashCode(),
      other.comments.hashCode());
  }
  
  public ClassBodyDeclarationWithComments withValue(hydra.ext.java.syntax.ClassBodyDeclaration value) {
    return new ClassBodyDeclarationWithComments(value, comments);
  }
  
  public ClassBodyDeclarationWithComments withComments(hydra.util.Maybe<String> comments) {
    return new ClassBodyDeclarationWithComments(value, comments);
  }
}
