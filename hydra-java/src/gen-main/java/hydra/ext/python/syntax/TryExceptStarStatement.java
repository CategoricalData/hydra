// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TryExceptStarStatement implements Serializable, Comparable<TryExceptStarStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TryExceptStarStatement");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_EXCEPTS = new hydra.core.Name("excepts");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public static final hydra.core.Name FIELD_NAME_FINALLY = new hydra.core.Name("finally");
  
  public final hydra.ext.python.syntax.Block body;
  
  public final java.util.List<hydra.ext.python.syntax.ExceptStarBlock> excepts;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Block> else_;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Block> finally_;
  
  public TryExceptStarStatement (hydra.ext.python.syntax.Block body, java.util.List<hydra.ext.python.syntax.ExceptStarBlock> excepts, hydra.util.Maybe<hydra.ext.python.syntax.Block> else_, hydra.util.Maybe<hydra.ext.python.syntax.Block> finally_) {
    this.body = body;
    this.excepts = excepts;
    this.else_ = else_;
    this.finally_ = finally_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryExceptStarStatement)) {
      return false;
    }
    TryExceptStarStatement o = (TryExceptStarStatement) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.excepts,
      o.excepts) && java.util.Objects.equals(
      this.else_,
      o.else_) && java.util.Objects.equals(
      this.finally_,
      o.finally_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(excepts) + 5 * java.util.Objects.hashCode(else_) + 7 * java.util.Objects.hashCode(finally_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TryExceptStarStatement other) {
    int cmp = 0;
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      excepts.hashCode(),
      other.excepts.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      else_.hashCode(),
      other.else_.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      finally_.hashCode(),
      other.finally_.hashCode());
  }
  
  public TryExceptStarStatement withBody(hydra.ext.python.syntax.Block body) {
    return new TryExceptStarStatement(body, excepts, else_, finally_);
  }
  
  public TryExceptStarStatement withExcepts(java.util.List<hydra.ext.python.syntax.ExceptStarBlock> excepts) {
    return new TryExceptStarStatement(body, excepts, else_, finally_);
  }
  
  public TryExceptStarStatement withElse(hydra.util.Maybe<hydra.ext.python.syntax.Block> else_) {
    return new TryExceptStarStatement(body, excepts, else_, finally_);
  }
  
  public TryExceptStarStatement withFinally(hydra.util.Maybe<hydra.ext.python.syntax.Block> finally_) {
    return new TryExceptStarStatement(body, excepts, else_, finally_);
  }
}
