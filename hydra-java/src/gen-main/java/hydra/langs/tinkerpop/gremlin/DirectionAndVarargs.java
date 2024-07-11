// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class DirectionAndVarargs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.DirectionAndVarargs");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalDirectionArgument direction;
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> varargs;
  
  public DirectionAndVarargs (hydra.langs.tinkerpop.gremlin.TraversalDirectionArgument direction, java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> varargs) {
    java.util.Objects.requireNonNull((direction));
    java.util.Objects.requireNonNull((varargs));
    this.direction = direction;
    this.varargs = varargs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DirectionAndVarargs)) {
      return false;
    }
    DirectionAndVarargs o = (DirectionAndVarargs) (other);
    return direction.equals(o.direction) && varargs.equals(o.varargs);
  }
  
  @Override
  public int hashCode() {
    return 2 * direction.hashCode() + 3 * varargs.hashCode();
  }
  
  public DirectionAndVarargs withDirection(hydra.langs.tinkerpop.gremlin.TraversalDirectionArgument direction) {
    java.util.Objects.requireNonNull((direction));
    return new DirectionAndVarargs(direction, varargs);
  }
  
  public DirectionAndVarargs withVarargs(java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> varargs) {
    java.util.Objects.requireNonNull((varargs));
    return new DirectionAndVarargs(direction, varargs);
  }
}