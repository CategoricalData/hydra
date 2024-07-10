// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class FunctionExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.FunctionExpression");
  
  public final hydra.langs.kusto.kql.Function function;
  
  public final java.util.List<hydra.langs.kusto.kql.Expression> arguments;
  
  public FunctionExpression (hydra.langs.kusto.kql.Function function, java.util.List<hydra.langs.kusto.kql.Expression> arguments) {
    if (function == null) {
      throw new IllegalArgumentException("null value for 'function' argument");
    }
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
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
    if (function == null) {
      throw new IllegalArgumentException("null value for 'function' argument");
    }
    return new FunctionExpression(function, arguments);
  }
  
  public FunctionExpression withArguments(java.util.List<hydra.langs.kusto.kql.Expression> arguments) {
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    return new FunctionExpression(function, arguments);
  }
}