// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalStringArgumentAndNestedTraversal implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.OptionalStringArgumentAndNestedTraversal");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> string;
  
  public final hydra.langs.tinkerpop.gremlin.NestedTraversal traversal;
  
  public OptionalStringArgumentAndNestedTraversal (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> string, hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    if (traversal == null) {
      throw new IllegalArgumentException("null value for 'traversal' argument");
    }
    this.string = string;
    this.traversal = traversal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OptionalStringArgumentAndNestedTraversal)) {
      return false;
    }
    OptionalStringArgumentAndNestedTraversal o = (OptionalStringArgumentAndNestedTraversal) (other);
    return string.equals(o.string) && traversal.equals(o.traversal);
  }
  
  @Override
  public int hashCode() {
    return 2 * string.hashCode() + 3 * traversal.hashCode();
  }
  
  public OptionalStringArgumentAndNestedTraversal withString(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> string) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    return new OptionalStringArgumentAndNestedTraversal(string, traversal);
  }
  
  public OptionalStringArgumentAndNestedTraversal withTraversal(hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    if (traversal == null) {
      throw new IllegalArgumentException("null value for 'traversal' argument");
    }
    return new OptionalStringArgumentAndNestedTraversal(string, traversal);
  }
}