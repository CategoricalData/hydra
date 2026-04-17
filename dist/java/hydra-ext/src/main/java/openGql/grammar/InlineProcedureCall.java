// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class InlineProcedureCall implements Serializable, Comparable<InlineProcedureCall> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.InlineProcedureCall");

  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");

  public static final hydra.core.Name NESTED = new hydra.core.Name("nested");

  public final hydra.util.Maybe<hydra.util.Maybe<java.util.List<String>>> scope;

  public final openGql.grammar.ProcedureBody nested;

  public InlineProcedureCall (hydra.util.Maybe<hydra.util.Maybe<java.util.List<String>>> scope, openGql.grammar.ProcedureBody nested) {
    this.scope = scope;
    this.nested = nested;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineProcedureCall)) {
      return false;
    }
    InlineProcedureCall o = (InlineProcedureCall) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.nested,
      o.nested);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(nested);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InlineProcedureCall other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      scope,
      other.scope);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nested,
      other.nested);
  }

  public InlineProcedureCall withScope(hydra.util.Maybe<hydra.util.Maybe<java.util.List<String>>> scope) {
    return new InlineProcedureCall(scope, nested);
  }

  public InlineProcedureCall withNested(openGql.grammar.ProcedureBody nested) {
    return new InlineProcedureCall(scope, nested);
  }
}
