// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A data constructor together with any comments
 */
public class ConstructorWithComments implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.ConstructorWithComments");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_COMMENTS = new hydra.core.Name("comments");
  
  public final hydra.ext.haskell.ast.Constructor body;
  
  public final hydra.util.Opt<String> comments;
  
  public ConstructorWithComments (hydra.ext.haskell.ast.Constructor body, hydra.util.Opt<String> comments) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((comments));
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
    java.util.Objects.requireNonNull((body));
    return new ConstructorWithComments(body, comments);
  }
  
  public ConstructorWithComments withComments(hydra.util.Opt<String> comments) {
    java.util.Objects.requireNonNull((comments));
    return new ConstructorWithComments(body, comments);
  }
}
