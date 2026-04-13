// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ConstructorBody implements Serializable, Comparable<ConstructorBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ConstructorBody");

  public static final hydra.core.Name INVOCATION = new hydra.core.Name("invocation");

  public static final hydra.core.Name STATEMENTS = new hydra.core.Name("statements");

  public final hydra.util.Maybe<hydra.java.syntax.ExplicitConstructorInvocation> invocation;

  public final java.util.List<hydra.java.syntax.BlockStatement> statements;

  public ConstructorBody (hydra.util.Maybe<hydra.java.syntax.ExplicitConstructorInvocation> invocation, java.util.List<hydra.java.syntax.BlockStatement> statements) {
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
    cmp = hydra.util.Comparing.compare(
      invocation,
      other.invocation);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      statements,
      other.statements);
  }

  public ConstructorBody withInvocation(hydra.util.Maybe<hydra.java.syntax.ExplicitConstructorInvocation> invocation) {
    return new ConstructorBody(invocation, statements);
  }

  public ConstructorBody withStatements(java.util.List<hydra.java.syntax.BlockStatement> statements) {
    return new ConstructorBody(invocation, statements);
  }
}
