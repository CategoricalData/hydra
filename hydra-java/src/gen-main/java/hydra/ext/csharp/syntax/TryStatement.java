// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class TryStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TryStatement");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_CATCHES = new hydra.core.Name("catches");
  
  public static final hydra.core.Name FIELD_NAME_FINALLY = new hydra.core.Name("finally");
  
  public final hydra.ext.csharp.syntax.Block body;
  
  public final hydra.ext.csharp.syntax.CatchClauses catches;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Block> finally_;
  
  public TryStatement (hydra.ext.csharp.syntax.Block body, hydra.ext.csharp.syntax.CatchClauses catches, hydra.util.Opt<hydra.ext.csharp.syntax.Block> finally_) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((catches));
    java.util.Objects.requireNonNull((finally_));
    this.body = body;
    this.catches = catches;
    this.finally_ = finally_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryStatement)) {
      return false;
    }
    TryStatement o = (TryStatement) (other);
    return body.equals(o.body) && catches.equals(o.catches) && finally_.equals(o.finally_);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * catches.hashCode() + 5 * finally_.hashCode();
  }
  
  public TryStatement withBody(hydra.ext.csharp.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new TryStatement(body, catches, finally_);
  }
  
  public TryStatement withCatches(hydra.ext.csharp.syntax.CatchClauses catches) {
    java.util.Objects.requireNonNull((catches));
    return new TryStatement(body, catches, finally_);
  }
  
  public TryStatement withFinally(hydra.util.Opt<hydra.ext.csharp.syntax.Block> finally_) {
    java.util.Objects.requireNonNull((finally_));
    return new TryStatement(body, catches, finally_);
  }
}