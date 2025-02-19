// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class DoStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DoStatement");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_WHILE = new hydra.core.Name("while");
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement body;
  
  public final hydra.ext.csharp.syntax.BooleanExpression while_;
  
  public DoStatement (hydra.ext.csharp.syntax.EmbeddedStatement body, hydra.ext.csharp.syntax.BooleanExpression while_) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((while_));
    this.body = body;
    this.while_ = while_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DoStatement)) {
      return false;
    }
    DoStatement o = (DoStatement) (other);
    return body.equals(o.body) && while_.equals(o.while_);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * while_.hashCode();
  }
  
  public DoStatement withBody(hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((body));
    return new DoStatement(body, while_);
  }
  
  public DoStatement withWhile(hydra.ext.csharp.syntax.BooleanExpression while_) {
    java.util.Objects.requireNonNull((while_));
    return new DoStatement(body, while_);
  }
}