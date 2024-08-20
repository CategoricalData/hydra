// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class DirectionAndVarargs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.DirectionAndVarargs");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTION = new hydra.core.Name("direction");
  
  public static final hydra.core.Name FIELD_NAME_VARARGS = new hydra.core.Name("varargs");
  
  public final hydra.ext.tinkerpop.gremlin.TraversalDirectionArgument direction;
  
  public final java.util.List<hydra.ext.tinkerpop.gremlin.StringNullableArgument> varargs;
  
  public DirectionAndVarargs (hydra.ext.tinkerpop.gremlin.TraversalDirectionArgument direction, java.util.List<hydra.ext.tinkerpop.gremlin.StringNullableArgument> varargs) {
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
  
  public DirectionAndVarargs withDirection(hydra.ext.tinkerpop.gremlin.TraversalDirectionArgument direction) {
    java.util.Objects.requireNonNull((direction));
    return new DirectionAndVarargs(direction, varargs);
  }
  
  public DirectionAndVarargs withVarargs(java.util.List<hydra.ext.tinkerpop.gremlin.StringNullableArgument> varargs) {
    java.util.Objects.requireNonNull((varargs));
    return new DirectionAndVarargs(direction, varargs);
  }
}
