// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class DirectionAndVarargs implements Serializable, Comparable<DirectionAndVarargs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DirectionAndVarargs");
  
  public static final hydra.core.Name DIRECTION = new hydra.core.Name("direction");
  
  public static final hydra.core.Name VARARGS = new hydra.core.Name("varargs");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirectionArgument direction;
  
  public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> varargs;
  
  public DirectionAndVarargs (hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirectionArgument direction, hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> varargs) {
    this.direction = direction;
    this.varargs = varargs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DirectionAndVarargs)) {
      return false;
    }
    DirectionAndVarargs o = (DirectionAndVarargs) other;
    return java.util.Objects.equals(
      this.direction,
      o.direction) && java.util.Objects.equals(
      this.varargs,
      o.varargs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(direction) + 3 * java.util.Objects.hashCode(varargs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DirectionAndVarargs other) {
    int cmp = 0;
    cmp = ((Comparable) direction).compareTo(other.direction);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) varargs).compareTo(other.varargs);
  }
  
  public DirectionAndVarargs withDirection(hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirectionArgument direction) {
    return new DirectionAndVarargs(direction, varargs);
  }
  
  public DirectionAndVarargs withVarargs(hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> varargs) {
    return new DirectionAndVarargs(direction, varargs);
  }
}
