// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TryExceptStatement implements Serializable, Comparable<TryExceptStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.TryExceptStatement");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name EXCEPTS = new hydra.core.Name("excepts");

  public static final hydra.core.Name ELSE = new hydra.core.Name("else");

  public static final hydra.core.Name FINALLY = new hydra.core.Name("finally");

  public final hydra.ext.python.syntax.Block body;

  public final java.util.List<hydra.ext.python.syntax.ExceptBlock> excepts;

  public final hydra.util.Maybe<hydra.ext.python.syntax.Block> else_;

  public final hydra.util.Maybe<hydra.ext.python.syntax.Block> finally_;

  public TryExceptStatement (hydra.ext.python.syntax.Block body, java.util.List<hydra.ext.python.syntax.ExceptBlock> excepts, hydra.util.Maybe<hydra.ext.python.syntax.Block> else_, hydra.util.Maybe<hydra.ext.python.syntax.Block> finally_) {
    this.body = body;
    this.excepts = excepts;
    this.else_ = else_;
    this.finally_ = finally_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryExceptStatement)) {
      return false;
    }
    TryExceptStatement o = (TryExceptStatement) other;
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
  public int compareTo(TryExceptStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      excepts,
      other.excepts);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      else_,
      other.else_);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      finally_,
      other.finally_);
  }

  public TryExceptStatement withBody(hydra.ext.python.syntax.Block body) {
    return new TryExceptStatement(body, excepts, else_, finally_);
  }

  public TryExceptStatement withExcepts(java.util.List<hydra.ext.python.syntax.ExceptBlock> excepts) {
    return new TryExceptStatement(body, excepts, else_, finally_);
  }

  public TryExceptStatement withElse(hydra.util.Maybe<hydra.ext.python.syntax.Block> else_) {
    return new TryExceptStatement(body, excepts, else_, finally_);
  }

  public TryExceptStatement withFinally(hydra.util.Maybe<hydra.ext.python.syntax.Block> finally_) {
    return new TryExceptStatement(body, excepts, else_, finally_);
  }
}
