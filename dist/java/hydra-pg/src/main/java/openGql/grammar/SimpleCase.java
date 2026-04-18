// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SimpleCase implements Serializable, Comparable<SimpleCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimpleCase");

  public static final hydra.core.Name CASE_OPERAND = new hydra.core.Name("caseOperand");

  public static final hydra.core.Name WHEN_CLAUSES = new hydra.core.Name("whenClauses");

  public static final hydra.core.Name ELSE_CLAUSE = new hydra.core.Name("elseClause");

  public final openGql.grammar.CaseOperand caseOperand;

  public final java.util.List<openGql.grammar.SimpleWhenClause> whenClauses;

  public final hydra.util.Maybe<openGql.grammar.Result> elseClause;

  public SimpleCase (openGql.grammar.CaseOperand caseOperand, java.util.List<openGql.grammar.SimpleWhenClause> whenClauses, hydra.util.Maybe<openGql.grammar.Result> elseClause) {
    this.caseOperand = caseOperand;
    this.whenClauses = whenClauses;
    this.elseClause = elseClause;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleCase)) {
      return false;
    }
    SimpleCase o = (SimpleCase) other;
    return java.util.Objects.equals(
      this.caseOperand,
      o.caseOperand) && java.util.Objects.equals(
      this.whenClauses,
      o.whenClauses) && java.util.Objects.equals(
      this.elseClause,
      o.elseClause);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(caseOperand) + 3 * java.util.Objects.hashCode(whenClauses) + 5 * java.util.Objects.hashCode(elseClause);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SimpleCase other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      caseOperand,
      other.caseOperand);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      whenClauses,
      other.whenClauses);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      elseClause,
      other.elseClause);
  }

  public SimpleCase withCaseOperand(openGql.grammar.CaseOperand caseOperand) {
    return new SimpleCase(caseOperand, whenClauses, elseClause);
  }

  public SimpleCase withWhenClauses(java.util.List<openGql.grammar.SimpleWhenClause> whenClauses) {
    return new SimpleCase(caseOperand, whenClauses, elseClause);
  }

  public SimpleCase withElseClause(hydra.util.Maybe<openGql.grammar.Result> elseClause) {
    return new SimpleCase(caseOperand, whenClauses, elseClause);
  }
}
