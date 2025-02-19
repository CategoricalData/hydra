// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class PopStringsArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PopStringsArgument");
  
  public static final hydra.core.Name FIELD_NAME_POP = new hydra.core.Name("pop");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop;
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> string;
  
  public PopStringsArgument (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop, java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> string) {
    java.util.Objects.requireNonNull((pop));
    java.util.Objects.requireNonNull((string));
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
  
  public PopStringsArgument withPop(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument pop) {
    java.util.Objects.requireNonNull((pop));
    return new PopStringsArgument(pop, string);
  }
  
  public PopStringsArgument withString(java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> string) {
    java.util.Objects.requireNonNull((string));
    return new PopStringsArgument(pop, string);
  }
}