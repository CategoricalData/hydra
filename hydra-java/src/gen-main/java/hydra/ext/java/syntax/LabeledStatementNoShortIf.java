// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class LabeledStatementNoShortIf implements Serializable, Comparable<LabeledStatementNoShortIf> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.LabeledStatementNoShortIf");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENT = new hydra.core.Name("statement");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.ext.java.syntax.StatementNoShortIf statement;
  
  public LabeledStatementNoShortIf (hydra.ext.java.syntax.Identifier identifier, hydra.ext.java.syntax.StatementNoShortIf statement) {
    this.identifier = identifier;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledStatementNoShortIf)) {
      return false;
    }
    LabeledStatementNoShortIf o = (LabeledStatementNoShortIf) other;
    return java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.statement,
      o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(identifier) + 3 * java.util.Objects.hashCode(statement);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LabeledStatementNoShortIf other) {
    int cmp = 0;
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) statement).compareTo(other.statement);
  }
  
  public LabeledStatementNoShortIf withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new LabeledStatementNoShortIf(identifier, statement);
  }
  
  public LabeledStatementNoShortIf withStatement(hydra.ext.java.syntax.StatementNoShortIf statement) {
    return new LabeledStatementNoShortIf(identifier, statement);
  }
}
