// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class AnonymousMethodExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AnonymousMethodExpression");
  
  public static final hydra.core.Name FIELD_NAME_ASYNC = new hydra.core.Name("async");
  
  public static final hydra.core.Name FIELD_NAME_SIGNATURE = new hydra.core.Name("signature");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final Boolean async;
  
  public final java.util.List<hydra.ext.csharp.syntax.ExplicitAnonymousFunctionParameter> signature;
  
  public final hydra.ext.csharp.syntax.Block body;
  
  public AnonymousMethodExpression (Boolean async, java.util.List<hydra.ext.csharp.syntax.ExplicitAnonymousFunctionParameter> signature, hydra.ext.csharp.syntax.Block body) {
    java.util.Objects.requireNonNull((async));
    java.util.Objects.requireNonNull((signature));
    java.util.Objects.requireNonNull((body));
    this.async = async;
    this.signature = signature;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnonymousMethodExpression)) {
      return false;
    }
    AnonymousMethodExpression o = (AnonymousMethodExpression) (other);
    return async.equals(o.async) && signature.equals(o.signature) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * async.hashCode() + 3 * signature.hashCode() + 5 * body.hashCode();
  }
  
  public AnonymousMethodExpression withAsync(Boolean async) {
    java.util.Objects.requireNonNull((async));
    return new AnonymousMethodExpression(async, signature, body);
  }
  
  public AnonymousMethodExpression withSignature(java.util.List<hydra.ext.csharp.syntax.ExplicitAnonymousFunctionParameter> signature) {
    java.util.Objects.requireNonNull((signature));
    return new AnonymousMethodExpression(async, signature, body);
  }
  
  public AnonymousMethodExpression withBody(hydra.ext.csharp.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new AnonymousMethodExpression(async, signature, body);
  }
}