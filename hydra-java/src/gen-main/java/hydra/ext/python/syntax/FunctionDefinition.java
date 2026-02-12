// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class FunctionDefinition implements Serializable, Comparable<FunctionDefinition> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.FunctionDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DECORATORS = new hydra.core.Name("decorators");
  
  public static final hydra.core.Name FIELD_NAME_RAW = new hydra.core.Name("raw");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decorators;
  
  public final hydra.ext.python.syntax.FunctionDefRaw raw;
  
  public FunctionDefinition (hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decorators, hydra.ext.python.syntax.FunctionDefRaw raw) {
    this.decorators = decorators;
    this.raw = raw;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionDefinition)) {
      return false;
    }
    FunctionDefinition o = (FunctionDefinition) other;
    return java.util.Objects.equals(
      this.decorators,
      o.decorators) && java.util.Objects.equals(
      this.raw,
      o.raw);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(decorators) + 3 * java.util.Objects.hashCode(raw);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionDefinition other) {
    int cmp = 0;
    cmp = Integer.compare(
      decorators.hashCode(),
      other.decorators.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) raw).compareTo(other.raw);
  }
  
  public FunctionDefinition withDecorators(hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decorators) {
    return new FunctionDefinition(decorators, raw);
  }
  
  public FunctionDefinition withRaw(hydra.ext.python.syntax.FunctionDefRaw raw) {
    return new FunctionDefinition(decorators, raw);
  }
}
