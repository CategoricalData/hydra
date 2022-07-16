package hydra.ext.java.syntax;

public class ClassBodyDeclarationWithComments {
  public final ClassBodyDeclaration value;
  
  public final java.util.Optional<String> comments;
  
  public ClassBodyDeclarationWithComments (ClassBodyDeclaration value, java.util.Optional<String> comments) {
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
  
  public ClassBodyDeclarationWithComments withValue(ClassBodyDeclaration value) {
    return new ClassBodyDeclarationWithComments(value, comments);
  }
  
  public ClassBodyDeclarationWithComments withComments(java.util.Optional<String> comments) {
    return new ClassBodyDeclarationWithComments(value, comments);
  }
}