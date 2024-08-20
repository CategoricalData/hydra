// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ProjectionItem implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.ProjectionItem");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public final hydra.ext.cypher.openCypher.Expression expression;
  
  public final hydra.util.Opt<hydra.ext.cypher.openCypher.Variable> variable;
  
  public ProjectionItem (hydra.ext.cypher.openCypher.Expression expression, hydra.util.Opt<hydra.ext.cypher.openCypher.Variable> variable) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((variable));
    this.expression = expression;
    this.variable = variable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProjectionItem)) {
      return false;
    }
    ProjectionItem o = (ProjectionItem) (other);
    return expression.equals(o.expression) && variable.equals(o.variable);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * variable.hashCode();
  }
  
  public ProjectionItem withExpression(hydra.ext.cypher.openCypher.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ProjectionItem(expression, variable);
  }
  
  public ProjectionItem withVariable(hydra.util.Opt<hydra.ext.cypher.openCypher.Variable> variable) {
    java.util.Objects.requireNonNull((variable));
    return new ProjectionItem(expression, variable);
  }
}
