package hydra.ext.haskell.ast;

/**
 * A field together with any comments
 */
public class FieldWithComments {
  public final Field field;
  
  public final java.util.Optional<String> comments;
  
  public FieldWithComments (Field field, java.util.Optional<String> comments) {
    this.field = field;
    this.comments = comments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldWithComments)) {
      return false;
    }
    FieldWithComments o = (FieldWithComments) (other);
    return field.equals(o.field) && comments.equals(o.comments);
  }
  
  @Override
  public int hashCode() {
    return 2 * field.hashCode() + 3 * comments.hashCode();
  }
  
  public FieldWithComments withField(Field field) {
    return new FieldWithComments(field, comments);
  }
  
  public FieldWithComments withComments(java.util.Optional<String> comments) {
    return new FieldWithComments(field, comments);
  }
}