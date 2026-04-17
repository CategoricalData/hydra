// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NamedProcedureCall implements Serializable, Comparable<NamedProcedureCall> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NamedProcedureCall");

  public static final hydra.core.Name REFERENCE = new hydra.core.Name("reference");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  public static final hydra.core.Name YIELD = new hydra.core.Name("yield");

  public final openGql.grammar.ProcedureReference reference;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.ValueExpression>> arguments;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.YieldItem>> yield;

  public NamedProcedureCall (openGql.grammar.ProcedureReference reference, hydra.util.Maybe<java.util.List<openGql.grammar.ValueExpression>> arguments, hydra.util.Maybe<java.util.List<openGql.grammar.YieldItem>> yield) {
    this.reference = reference;
    this.arguments = arguments;
    this.yield = yield;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedProcedureCall)) {
      return false;
    }
    NamedProcedureCall o = (NamedProcedureCall) other;
    return java.util.Objects.equals(
      this.reference,
      o.reference) && java.util.Objects.equals(
      this.arguments,
      o.arguments) && java.util.Objects.equals(
      this.yield,
      o.yield);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(reference) + 3 * java.util.Objects.hashCode(arguments) + 5 * java.util.Objects.hashCode(yield);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NamedProcedureCall other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      reference,
      other.reference);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      arguments,
      other.arguments);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      yield,
      other.yield);
  }

  public NamedProcedureCall withReference(openGql.grammar.ProcedureReference reference) {
    return new NamedProcedureCall(reference, arguments, yield);
  }

  public NamedProcedureCall withArguments(hydra.util.Maybe<java.util.List<openGql.grammar.ValueExpression>> arguments) {
    return new NamedProcedureCall(reference, arguments, yield);
  }

  public NamedProcedureCall withYield(hydra.util.Maybe<java.util.List<openGql.grammar.YieldItem>> yield) {
    return new NamedProcedureCall(reference, arguments, yield);
  }
}
