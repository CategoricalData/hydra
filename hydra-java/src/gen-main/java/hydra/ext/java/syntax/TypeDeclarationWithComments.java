package hydra.ext.java.syntax;

public class TypeDeclarationWithComments {
  public final TypeDeclaration value;
  
  public final java.util.Optional<String> comments;
  
  public TypeDeclarationWithComments (TypeDeclaration value, java.util.Optional<String> comments) {
    this.value = value;
    this.comments = comments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeDeclarationWithComments)) {
      return false;
    }
    TypeDeclarationWithComments o = (TypeDeclarationWithComments) (other);
    return value.equals(o.value) && comments.equals(o.comments);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * comments.hashCode();
  }
  
  public TypeDeclarationWithComments withValue(TypeDeclaration value) {
    return new TypeDeclarationWithComments(value, comments);
  }
  
  public TypeDeclarationWithComments withComments(java.util.Optional<String> comments) {
    return new TypeDeclarationWithComments(value, comments);
  }
}