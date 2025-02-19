// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class FunctionDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.FunctionDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DECORATORS = new hydra.core.Name("decorators");
  
  public static final hydra.core.Name FIELD_NAME_RAW = new hydra.core.Name("raw");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Decorators> decorators;
  
  public final hydra.ext.python.syntax.FunctionDefRaw raw;
  
  public FunctionDefinition (hydra.util.Opt<hydra.ext.python.syntax.Decorators> decorators, hydra.ext.python.syntax.FunctionDefRaw raw) {
    java.util.Objects.requireNonNull((decorators));
    java.util.Objects.requireNonNull((raw));
    this.decorators = decorators;
    this.raw = raw;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionDefinition)) {
      return false;
    }
    FunctionDefinition o = (FunctionDefinition) (other);
    return decorators.equals(o.decorators) && raw.equals(o.raw);
  }
  
  @Override
  public int hashCode() {
    return 2 * decorators.hashCode() + 3 * raw.hashCode();
  }
  
  public FunctionDefinition withDecorators(hydra.util.Opt<hydra.ext.python.syntax.Decorators> decorators) {
    java.util.Objects.requireNonNull((decorators));
    return new FunctionDefinition(decorators, raw);
  }
  
  public FunctionDefinition withRaw(hydra.ext.python.syntax.FunctionDefRaw raw) {
    java.util.Objects.requireNonNull((raw));
    return new FunctionDefinition(decorators, raw);
  }
}