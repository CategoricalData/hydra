package hydra.langs.kusto.kql;

import java.io.Serializable;

public class FunctionExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.FunctionExpression");
  
  public final hydra.langs.kusto.kql.Function function;
  
  public final java.util.List<hydra.langs.kusto.kql.Expression> arguments;
  
  public FunctionExpression (hydra.langs.kusto.kql.Function function, java.util.List<hydra.langs.kusto.kql.Expression> arguments) {
    this.function = function;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionExpression)) {
      return false;
    }
    FunctionExpression o = (FunctionExpression) (other);
    return function.equals(o.function) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode() + 3 * arguments.hashCode();
  }
  
  public FunctionExpression withFunction(hydra.langs.kusto.kql.Function function) {
    return new FunctionExpression(function, arguments);
  }
  
  public FunctionExpression withArguments(java.util.List<hydra.langs.kusto.kql.Expression> arguments) {
    return new FunctionExpression(function, arguments);
  }
}