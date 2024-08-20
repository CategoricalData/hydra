// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ConstructorBody implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.ConstructorBody");
  
  public static final hydra.core.Name FIELD_NAME_INVOCATION = new hydra.core.Name("invocation");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public final hydra.util.Opt<hydra.ext.java.syntax.ExplicitConstructorInvocation> invocation;
  
  public final java.util.List<hydra.ext.java.syntax.BlockStatement> statements;
  
  public ConstructorBody (hydra.util.Opt<hydra.ext.java.syntax.ExplicitConstructorInvocation> invocation, java.util.List<hydra.ext.java.syntax.BlockStatement> statements) {
    java.util.Objects.requireNonNull((invocation));
    java.util.Objects.requireNonNull((statements));
    this.invocation = invocation;
    this.statements = statements;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorBody)) {
      return false;
    }
    ConstructorBody o = (ConstructorBody) (other);
    return invocation.equals(o.invocation) && statements.equals(o.statements);
  }
  
  @Override
  public int hashCode() {
    return 2 * invocation.hashCode() + 3 * statements.hashCode();
  }
  
  public ConstructorBody withInvocation(hydra.util.Opt<hydra.ext.java.syntax.ExplicitConstructorInvocation> invocation) {
    java.util.Objects.requireNonNull((invocation));
    return new ConstructorBody(invocation, statements);
  }
  
  public ConstructorBody withStatements(java.util.List<hydra.ext.java.syntax.BlockStatement> statements) {
    java.util.Objects.requireNonNull((statements));
    return new ConstructorBody(invocation, statements);
  }
}
