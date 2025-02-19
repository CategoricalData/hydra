// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TryExceptStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TryExceptStatement");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_EXCEPTS = new hydra.core.Name("excepts");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public static final hydra.core.Name FIELD_NAME_FINALLY = new hydra.core.Name("finally");
  
  public final hydra.ext.python.syntax.Block body;
  
  public final java.util.List<hydra.ext.python.syntax.ExceptBlock> excepts;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Block> else_;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Block> finally_;
  
  public TryExceptStatement (hydra.ext.python.syntax.Block body, java.util.List<hydra.ext.python.syntax.ExceptBlock> excepts, hydra.util.Opt<hydra.ext.python.syntax.Block> else_, hydra.util.Opt<hydra.ext.python.syntax.Block> finally_) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((excepts));
    java.util.Objects.requireNonNull((else_));
    java.util.Objects.requireNonNull((finally_));
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
    TryExceptStatement o = (TryExceptStatement) (other);
    return body.equals(o.body) && excepts.equals(o.excepts) && else_.equals(o.else_) && finally_.equals(o.finally_);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * excepts.hashCode() + 5 * else_.hashCode() + 7 * finally_.hashCode();
  }
  
  public TryExceptStatement withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new TryExceptStatement(body, excepts, else_, finally_);
  }
  
  public TryExceptStatement withExcepts(java.util.List<hydra.ext.python.syntax.ExceptBlock> excepts) {
    java.util.Objects.requireNonNull((excepts));
    return new TryExceptStatement(body, excepts, else_, finally_);
  }
  
  public TryExceptStatement withElse(hydra.util.Opt<hydra.ext.python.syntax.Block> else_) {
    java.util.Objects.requireNonNull((else_));
    return new TryExceptStatement(body, excepts, else_, finally_);
  }
  
  public TryExceptStatement withFinally(hydra.util.Opt<hydra.ext.python.syntax.Block> finally_) {
    java.util.Objects.requireNonNull((finally_));
    return new TryExceptStatement(body, excepts, else_, finally_);
  }
}