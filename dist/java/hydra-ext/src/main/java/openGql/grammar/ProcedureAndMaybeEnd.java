// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ProcedureAndMaybeEnd implements Serializable, Comparable<ProcedureAndMaybeEnd> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ProcedureAndMaybeEnd");

  public static final hydra.core.Name PROCEDURE = new hydra.core.Name("procedure");

  public static final hydra.core.Name END = new hydra.core.Name("end");

  public final openGql.grammar.ProcedureBody procedure;

  public final hydra.util.Maybe<openGql.grammar.EndTransactionCommand> end;

  public ProcedureAndMaybeEnd (openGql.grammar.ProcedureBody procedure, hydra.util.Maybe<openGql.grammar.EndTransactionCommand> end) {
    this.procedure = procedure;
    this.end = end;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProcedureAndMaybeEnd)) {
      return false;
    }
    ProcedureAndMaybeEnd o = (ProcedureAndMaybeEnd) other;
    return java.util.Objects.equals(
      this.procedure,
      o.procedure) && java.util.Objects.equals(
      this.end,
      o.end);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(procedure) + 3 * java.util.Objects.hashCode(end);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ProcedureAndMaybeEnd other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      procedure,
      other.procedure);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      end,
      other.end);
  }

  public ProcedureAndMaybeEnd withProcedure(openGql.grammar.ProcedureBody procedure) {
    return new ProcedureAndMaybeEnd(procedure, end);
  }

  public ProcedureAndMaybeEnd withEnd(hydra.util.Maybe<openGql.grammar.EndTransactionCommand> end) {
    return new ProcedureAndMaybeEnd(procedure, end);
  }
}
