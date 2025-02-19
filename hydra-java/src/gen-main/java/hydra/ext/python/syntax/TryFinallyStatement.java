// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TryFinallyStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TryFinallyStatement");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_FINALLY = new hydra.core.Name("finally");
  
  public final hydra.ext.python.syntax.Block body;
  
  public final hydra.ext.python.syntax.Block finally_;
  
  public TryFinallyStatement (hydra.ext.python.syntax.Block body, hydra.ext.python.syntax.Block finally_) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((finally_));
    this.body = body;
    this.finally_ = finally_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryFinallyStatement)) {
      return false;
    }
    TryFinallyStatement o = (TryFinallyStatement) (other);
    return body.equals(o.body) && finally_.equals(o.finally_);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * finally_.hashCode();
  }
  
  public TryFinallyStatement withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new TryFinallyStatement(body, finally_);
  }
  
  public TryFinallyStatement withFinally(hydra.ext.python.syntax.Block finally_) {
    java.util.Objects.requireNonNull((finally_));
    return new TryFinallyStatement(body, finally_);
  }
}