// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class LambdaExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.LambdaExpression");
  
  public static final hydra.core.Name FIELD_NAME_ASYNC = new hydra.core.Name("async");
  
  public static final hydra.core.Name FIELD_NAME_SIGNATURE = new hydra.core.Name("signature");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final Boolean async;
  
  public final hydra.ext.csharp.syntax.AnonymousFunctionSignature signature;
  
  public final hydra.ext.csharp.syntax.AnonymousFunctionBody body;
  
  public LambdaExpression (Boolean async, hydra.ext.csharp.syntax.AnonymousFunctionSignature signature, hydra.ext.csharp.syntax.AnonymousFunctionBody body) {
    java.util.Objects.requireNonNull((async));
    java.util.Objects.requireNonNull((signature));
    java.util.Objects.requireNonNull((body));
    this.async = async;
    this.signature = signature;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaExpression)) {
      return false;
    }
    LambdaExpression o = (LambdaExpression) (other);
    return async.equals(o.async) && signature.equals(o.signature) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * async.hashCode() + 3 * signature.hashCode() + 5 * body.hashCode();
  }
  
  public LambdaExpression withAsync(Boolean async) {
    java.util.Objects.requireNonNull((async));
    return new LambdaExpression(async, signature, body);
  }
  
  public LambdaExpression withSignature(hydra.ext.csharp.syntax.AnonymousFunctionSignature signature) {
    java.util.Objects.requireNonNull((signature));
    return new LambdaExpression(async, signature, body);
  }
  
  public LambdaExpression withBody(hydra.ext.csharp.syntax.AnonymousFunctionBody body) {
    java.util.Objects.requireNonNull((body));
    return new LambdaExpression(async, signature, body);
  }
}