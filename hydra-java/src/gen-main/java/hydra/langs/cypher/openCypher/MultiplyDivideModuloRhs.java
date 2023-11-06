package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MultiplyDivideModuloRhs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MultiplyDivideModuloRhs");
  
  public final hydra.langs.cypher.openCypher.TimesOrDivideOrModulo operator;
  
  public final hydra.langs.cypher.openCypher.PowerOfExpression expression;
  
  public MultiplyDivideModuloRhs (hydra.langs.cypher.openCypher.TimesOrDivideOrModulo operator, hydra.langs.cypher.openCypher.PowerOfExpression expression) {
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiplyDivideModuloRhs)) {
      return false;
    }
    MultiplyDivideModuloRhs o = (MultiplyDivideModuloRhs) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public MultiplyDivideModuloRhs withOperator(hydra.langs.cypher.openCypher.TimesOrDivideOrModulo operator) {
    return new MultiplyDivideModuloRhs(operator, expression);
  }
  
  public MultiplyDivideModuloRhs withExpression(hydra.langs.cypher.openCypher.PowerOfExpression expression) {
    return new MultiplyDivideModuloRhs(operator, expression);
  }
}