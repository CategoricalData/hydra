// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ClassBodyDeclarationWithComments implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ClassBodyDeclarationWithComments");
  
  public final hydra.langs.java.syntax.ClassBodyDeclaration value;
  
  public final java.util.Optional<String> comments;
  
  public ClassBodyDeclarationWithComments (hydra.langs.java.syntax.ClassBodyDeclaration value, java.util.Optional<String> comments) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    if (comments == null) {
      throw new IllegalArgumentException("null value for 'comments' argument");
    }
    this.value = value;
    this.comments = comments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassBodyDeclarationWithComments)) {
      return false;
    }
    ClassBodyDeclarationWithComments o = (ClassBodyDeclarationWithComments) (other);
    return value.equals(o.value) && comments.equals(o.comments);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * comments.hashCode();
  }
  
  public ClassBodyDeclarationWithComments withValue(hydra.langs.java.syntax.ClassBodyDeclaration value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new ClassBodyDeclarationWithComments(value, comments);
  }
  
  public ClassBodyDeclarationWithComments withComments(java.util.Optional<String> comments) {
    if (comments == null) {
      throw new IllegalArgumentException("null value for 'comments' argument");
    }
    return new ClassBodyDeclarationWithComments(value, comments);
  }
}