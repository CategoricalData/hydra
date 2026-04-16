// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CallProcedureStatement implements Serializable, Comparable<CallProcedureStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CallProcedureStatement");

  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");

  public static final hydra.core.Name CALL = new hydra.core.Name("call");

  public final Boolean optional;

  public final openGql.grammar.ProcedureCall call;

  public CallProcedureStatement (Boolean optional, openGql.grammar.ProcedureCall call) {
    this.optional = optional;
    this.call = call;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CallProcedureStatement)) {
      return false;
    }
    CallProcedureStatement o = (CallProcedureStatement) other;
    return java.util.Objects.equals(
      this.optional,
      o.optional) && java.util.Objects.equals(
      this.call,
      o.call);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(optional) + 3 * java.util.Objects.hashCode(call);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CallProcedureStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      optional,
      other.optional);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      call,
      other.call);
  }

  public CallProcedureStatement withOptional(Boolean optional) {
    return new CallProcedureStatement(optional, call);
  }

  public CallProcedureStatement withCall(openGql.grammar.ProcedureCall call) {
    return new CallProcedureStatement(optional, call);
  }
}
