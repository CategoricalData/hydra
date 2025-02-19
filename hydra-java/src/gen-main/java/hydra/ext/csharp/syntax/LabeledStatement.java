// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class LabeledStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.LabeledStatement");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENT = new hydra.core.Name("statement");
  
  public final hydra.ext.csharp.syntax.Identifier label;
  
  public final hydra.ext.csharp.syntax.Statement statement;
  
  public LabeledStatement (hydra.ext.csharp.syntax.Identifier label, hydra.ext.csharp.syntax.Statement statement) {
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((statement));
    this.label = label;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledStatement)) {
      return false;
    }
    LabeledStatement o = (LabeledStatement) (other);
    return label.equals(o.label) && statement.equals(o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * statement.hashCode();
  }
  
  public LabeledStatement withLabel(hydra.ext.csharp.syntax.Identifier label) {
    java.util.Objects.requireNonNull((label));
    return new LabeledStatement(label, statement);
  }
  
  public LabeledStatement withStatement(hydra.ext.csharp.syntax.Statement statement) {
    java.util.Objects.requireNonNull((statement));
    return new LabeledStatement(label, statement);
  }
}