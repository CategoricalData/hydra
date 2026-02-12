// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class LabeledStatement implements Serializable, Comparable<LabeledStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.LabeledStatement");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENT = new hydra.core.Name("statement");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.ext.java.syntax.Statement statement;
  
  public LabeledStatement (hydra.ext.java.syntax.Identifier identifier, hydra.ext.java.syntax.Statement statement) {
    this.identifier = identifier;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledStatement)) {
      return false;
    }
    LabeledStatement o = (LabeledStatement) other;
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
  public int compareTo(LabeledStatement other) {
    int cmp = 0;
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) statement).compareTo(other.statement);
  }
  
  public LabeledStatement withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new LabeledStatement(identifier, statement);
  }
  
  public LabeledStatement withStatement(hydra.ext.java.syntax.Statement statement) {
    return new LabeledStatement(identifier, statement);
  }
}
