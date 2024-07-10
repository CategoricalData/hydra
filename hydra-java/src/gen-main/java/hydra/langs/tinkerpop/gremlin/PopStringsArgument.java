// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class PopStringsArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.PopStringsArgument");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalPopArgument pop;
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.StringArgument> string;
  
  public PopStringsArgument (hydra.langs.tinkerpop.gremlin.TraversalPopArgument pop, java.util.List<hydra.langs.tinkerpop.gremlin.StringArgument> string) {
    if (pop == null) {
      throw new IllegalArgumentException("null value for 'pop' argument");
    }
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    this.pop = pop;
    this.string = string;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PopStringsArgument)) {
      return false;
    }
    PopStringsArgument o = (PopStringsArgument) (other);
    return pop.equals(o.pop) && string.equals(o.string);
  }
  
  @Override
  public int hashCode() {
    return 2 * pop.hashCode() + 3 * string.hashCode();
  }
  
  public PopStringsArgument withPop(hydra.langs.tinkerpop.gremlin.TraversalPopArgument pop) {
    if (pop == null) {
      throw new IllegalArgumentException("null value for 'pop' argument");
    }
    return new PopStringsArgument(pop, string);
  }
  
  public PopStringsArgument withString(java.util.List<hydra.langs.tinkerpop.gremlin.StringArgument> string) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    return new PopStringsArgument(pop, string);
  }
}