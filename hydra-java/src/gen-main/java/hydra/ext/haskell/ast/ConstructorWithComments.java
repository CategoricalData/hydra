package hydra.ext.haskell.ast;

/**
 * A data constructor together with any comments
 */
public class ConstructorWithComments {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.ConstructorWithComments");
  
  public final hydra.ext.haskell.ast.Constructor body;
  
  public final java.util.Optional<String> comments;
  
  public ConstructorWithComments (hydra.ext.haskell.ast.Constructor body, java.util.Optional<String> comments) {
    this.body = body;
    this.comments = comments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorWithComments)) {
      return false;
    }
    ConstructorWithComments o = (ConstructorWithComments) (other);
    return body.equals(o.body) && comments.equals(o.comments);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * comments.hashCode();
  }
  
  public ConstructorWithComments withBody(hydra.ext.haskell.ast.Constructor body) {
    return new ConstructorWithComments(body, comments);
  }
  
  public ConstructorWithComments withComments(java.util.Optional<String> comments) {
    return new ConstructorWithComments(body, comments);
  }
}