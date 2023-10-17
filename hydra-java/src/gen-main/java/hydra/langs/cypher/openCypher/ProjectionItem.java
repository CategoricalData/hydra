package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProjectionItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProjectionItem");
  
  public final hydra.langs.cypher.openCypher.Expression expression;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Variable> alias;
  
  public ProjectionItem (hydra.langs.cypher.openCypher.Expression expression, java.util.Optional<hydra.langs.cypher.openCypher.Variable> alias) {
    this.expression = expression;
    this.alias = alias;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProjectionItem)) {
      return false;
    }
    ProjectionItem o = (ProjectionItem) (other);
    return expression.equals(o.expression) && alias.equals(o.alias);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * alias.hashCode();
  }
  
  public ProjectionItem withExpression(hydra.langs.cypher.openCypher.Expression expression) {
    return new ProjectionItem(expression, alias);
  }
  
  public ProjectionItem withAlias(java.util.Optional<hydra.langs.cypher.openCypher.Variable> alias) {
    return new ProjectionItem(expression, alias);
  }
}