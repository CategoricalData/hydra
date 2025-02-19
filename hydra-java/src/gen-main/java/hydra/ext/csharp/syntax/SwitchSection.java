// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class SwitchSection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SwitchSection");
  
  public static final hydra.core.Name FIELD_NAME_LABELS = new hydra.core.Name("labels");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public final java.util.List<hydra.ext.csharp.syntax.SwitchLabel> labels;
  
  public final java.util.List<hydra.ext.csharp.syntax.Statement> statements;
  
  public SwitchSection (java.util.List<hydra.ext.csharp.syntax.SwitchLabel> labels, java.util.List<hydra.ext.csharp.syntax.Statement> statements) {
    java.util.Objects.requireNonNull((labels));
    java.util.Objects.requireNonNull((statements));
    this.labels = labels;
    this.statements = statements;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchSection)) {
      return false;
    }
    SwitchSection o = (SwitchSection) (other);
    return labels.equals(o.labels) && statements.equals(o.statements);
  }
  
  @Override
  public int hashCode() {
    return 2 * labels.hashCode() + 3 * statements.hashCode();
  }
  
  public SwitchSection withLabels(java.util.List<hydra.ext.csharp.syntax.SwitchLabel> labels) {
    java.util.Objects.requireNonNull((labels));
    return new SwitchSection(labels, statements);
  }
  
  public SwitchSection withStatements(java.util.List<hydra.ext.csharp.syntax.Statement> statements) {
    java.util.Objects.requireNonNull((statements));
    return new SwitchSection(labels, statements);
  }
}