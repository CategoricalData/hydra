// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TryFinallyStatement implements Serializable, Comparable<TryFinallyStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TryFinallyStatement");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_FINALLY = new hydra.core.Name("finally");
  
  public final hydra.ext.python.syntax.Block body;
  
  public final hydra.ext.python.syntax.Block finally_;
  
  public TryFinallyStatement (hydra.ext.python.syntax.Block body, hydra.ext.python.syntax.Block finally_) {
    this.body = body;
    this.finally_ = finally_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryFinallyStatement)) {
      return false;
    }
    TryFinallyStatement o = (TryFinallyStatement) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.finally_,
      o.finally_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(finally_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TryFinallyStatement other) {
    int cmp = 0;
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) finally_).compareTo(other.finally_);
  }
  
  public TryFinallyStatement withBody(hydra.ext.python.syntax.Block body) {
    return new TryFinallyStatement(body, finally_);
  }
  
  public TryFinallyStatement withFinally(hydra.ext.python.syntax.Block finally_) {
    return new TryFinallyStatement(body, finally_);
  }
}
