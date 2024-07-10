// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProjectionItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProjectionItem");
  
  public final hydra.langs.cypher.openCypher.Expression expression;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable;
  
  public ProjectionItem (hydra.langs.cypher.openCypher.Expression expression, java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
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
  
  public ProjectionItem withExpression(hydra.langs.cypher.openCypher.Expression expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new ProjectionItem(expression, variable);
  }
  
  public ProjectionItem withVariable(java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable) {
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
    return new ProjectionItem(expression, variable);
  }
}