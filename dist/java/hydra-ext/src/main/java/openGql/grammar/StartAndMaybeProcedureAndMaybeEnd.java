// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class StartAndMaybeProcedureAndMaybeEnd implements Serializable, Comparable<StartAndMaybeProcedureAndMaybeEnd> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.StartAndMaybeProcedureAndMaybeEnd");

  public static final hydra.core.Name START = new hydra.core.Name("start");

  public static final hydra.core.Name PROCEDURE_AND_END = new hydra.core.Name("procedureAndEnd");

  public final hydra.util.Maybe<java.util.List<openGql.grammar.TransactionAccessMode>> start;

  public final hydra.util.Maybe<openGql.grammar.ProcedureAndMaybeEnd> procedureAndEnd;

  public StartAndMaybeProcedureAndMaybeEnd (hydra.util.Maybe<java.util.List<openGql.grammar.TransactionAccessMode>> start, hydra.util.Maybe<openGql.grammar.ProcedureAndMaybeEnd> procedureAndEnd) {
    this.start = start;
    this.procedureAndEnd = procedureAndEnd;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StartAndMaybeProcedureAndMaybeEnd)) {
      return false;
    }
    StartAndMaybeProcedureAndMaybeEnd o = (StartAndMaybeProcedureAndMaybeEnd) other;
    return java.util.Objects.equals(
      this.start,
      o.start) && java.util.Objects.equals(
      this.procedureAndEnd,
      o.procedureAndEnd);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(start) + 3 * java.util.Objects.hashCode(procedureAndEnd);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StartAndMaybeProcedureAndMaybeEnd other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      start,
      other.start);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      procedureAndEnd,
      other.procedureAndEnd);
  }

  public StartAndMaybeProcedureAndMaybeEnd withStart(hydra.util.Maybe<java.util.List<openGql.grammar.TransactionAccessMode>> start) {
    return new StartAndMaybeProcedureAndMaybeEnd(start, procedureAndEnd);
  }

  public StartAndMaybeProcedureAndMaybeEnd withProcedureAndEnd(hydra.util.Maybe<openGql.grammar.ProcedureAndMaybeEnd> procedureAndEnd) {
    return new StartAndMaybeProcedureAndMaybeEnd(start, procedureAndEnd);
  }
}
