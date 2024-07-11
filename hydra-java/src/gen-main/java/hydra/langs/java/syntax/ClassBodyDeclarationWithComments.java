// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ClassBodyDeclarationWithComments implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ClassBodyDeclarationWithComments");
  
  public final hydra.langs.java.syntax.ClassBodyDeclaration value;
  
  public final hydra.util.Opt<String> comments;
  
  public ClassBodyDeclarationWithComments (hydra.langs.java.syntax.ClassBodyDeclaration value, hydra.util.Opt<String> comments) {
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((comments));
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
    java.util.Objects.requireNonNull((value));
    return new ClassBodyDeclarationWithComments(value, comments);
  }
  
  public ClassBodyDeclarationWithComments withComments(hydra.util.Opt<String> comments) {
    java.util.Objects.requireNonNull((comments));
    return new ClassBodyDeclarationWithComments(value, comments);
  }
}