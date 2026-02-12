// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ConstructorBody implements Serializable, Comparable<ConstructorBody> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ConstructorBody");
  
  public static final hydra.core.Name FIELD_NAME_INVOCATION = new hydra.core.Name("invocation");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ExplicitConstructorInvocation> invocation;
  
  public final java.util.List<hydra.ext.java.syntax.BlockStatement> statements;
  
  public ConstructorBody (hydra.util.Maybe<hydra.ext.java.syntax.ExplicitConstructorInvocation> invocation, java.util.List<hydra.ext.java.syntax.BlockStatement> statements) {
    this.invocation = invocation;
    this.statements = statements;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorBody)) {
      return false;
    }
    ConstructorBody o = (ConstructorBody) other;
    return java.util.Objects.equals(
      this.invocation,
      o.invocation) && java.util.Objects.equals(
      this.statements,
      o.statements);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(invocation) + 3 * java.util.Objects.hashCode(statements);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstructorBody other) {
    int cmp = 0;
    cmp = Integer.compare(
      invocation.hashCode(),
      other.invocation.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      statements.hashCode(),
      other.statements.hashCode());
  }
  
  public ConstructorBody withInvocation(hydra.util.Maybe<hydra.ext.java.syntax.ExplicitConstructorInvocation> invocation) {
    return new ConstructorBody(invocation, statements);
  }
  
  public ConstructorBody withStatements(java.util.List<hydra.ext.java.syntax.BlockStatement> statements) {
    return new ConstructorBody(invocation, statements);
  }
}
