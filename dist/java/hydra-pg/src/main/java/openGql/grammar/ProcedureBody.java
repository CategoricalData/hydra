// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ProcedureBody implements Serializable, Comparable<ProcedureBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ProcedureBody");

  public static final hydra.core.Name AT_SCHEMA = new hydra.core.Name("atSchema");

  public static final hydra.core.Name BINDINGS = new hydra.core.Name("bindings");

  public static final hydra.core.Name STATEMENTS = new hydra.core.Name("statements");

  public final hydra.util.Maybe<openGql.grammar.SchemaReference> atSchema;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.BindingVariableDefinition>> bindings;

  public final openGql.grammar.StatementBlock statements;

  public ProcedureBody (hydra.util.Maybe<openGql.grammar.SchemaReference> atSchema, hydra.util.Maybe<java.util.List<openGql.grammar.BindingVariableDefinition>> bindings, openGql.grammar.StatementBlock statements) {
    this.atSchema = atSchema;
    this.bindings = bindings;
    this.statements = statements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProcedureBody)) {
      return false;
    }
    ProcedureBody o = (ProcedureBody) other;
    return java.util.Objects.equals(
      this.atSchema,
      o.atSchema) && java.util.Objects.equals(
      this.bindings,
      o.bindings) && java.util.Objects.equals(
      this.statements,
      o.statements);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(atSchema) + 3 * java.util.Objects.hashCode(bindings) + 5 * java.util.Objects.hashCode(statements);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ProcedureBody other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      atSchema,
      other.atSchema);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      bindings,
      other.bindings);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      statements,
      other.statements);
  }

  public ProcedureBody withAtSchema(hydra.util.Maybe<openGql.grammar.SchemaReference> atSchema) {
    return new ProcedureBody(atSchema, bindings, statements);
  }

  public ProcedureBody withBindings(hydra.util.Maybe<java.util.List<openGql.grammar.BindingVariableDefinition>> bindings) {
    return new ProcedureBody(atSchema, bindings, statements);
  }

  public ProcedureBody withStatements(openGql.grammar.StatementBlock statements) {
    return new ProcedureBody(atSchema, bindings, statements);
  }
}
