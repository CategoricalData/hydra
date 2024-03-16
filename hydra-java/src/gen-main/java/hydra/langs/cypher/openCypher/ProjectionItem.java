package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProjectionItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProjectionItem");
  
  public final hydra.langs.cypher.openCypher.Expression expression;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable;
  
  public ProjectionItem (hydra.langs.cypher.openCypher.Expression expression, java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable) {
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
    return new ProjectionItem(expression, variable);
  }
  
  public ProjectionItem withVariable(java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable) {
    return new ProjectionItem(expression, variable);
  }
}