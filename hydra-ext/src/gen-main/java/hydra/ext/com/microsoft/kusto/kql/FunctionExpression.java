// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class FunctionExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.FunctionExpression");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.com.microsoft.kusto.kql.Function function;
  
  public final java.util.List<hydra.ext.com.microsoft.kusto.kql.Expression> arguments;
  
  public FunctionExpression (hydra.ext.com.microsoft.kusto.kql.Function function, java.util.List<hydra.ext.com.microsoft.kusto.kql.Expression> arguments) {
    java.util.Objects.requireNonNull((function));
    java.util.Objects.requireNonNull((arguments));
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
  
  public FunctionExpression withFunction(hydra.ext.com.microsoft.kusto.kql.Function function) {
    java.util.Objects.requireNonNull((function));
    return new FunctionExpression(function, arguments);
  }
  
  public FunctionExpression withArguments(java.util.List<hydra.ext.com.microsoft.kusto.kql.Expression> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new FunctionExpression(function, arguments);
  }
}