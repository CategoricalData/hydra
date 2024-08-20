// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalStringArgumentAndNestedTraversal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.OptionalStringArgumentAndNestedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL = new hydra.core.Name("traversal");
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.StringArgument> string;
  
  public final hydra.ext.tinkerpop.gremlin.NestedTraversal traversal;
  
  public OptionalStringArgumentAndNestedTraversal (hydra.util.Opt<hydra.ext.tinkerpop.gremlin.StringArgument> string, hydra.ext.tinkerpop.gremlin.NestedTraversal traversal) {
    java.util.Objects.requireNonNull((string));
    java.util.Objects.requireNonNull((traversal));
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
  
  public OptionalStringArgumentAndNestedTraversal withString(hydra.util.Opt<hydra.ext.tinkerpop.gremlin.StringArgument> string) {
    java.util.Objects.requireNonNull((string));
    return new OptionalStringArgumentAndNestedTraversal(string, traversal);
  }
  
  public OptionalStringArgumentAndNestedTraversal withTraversal(hydra.ext.tinkerpop.gremlin.NestedTraversal traversal) {
    java.util.Objects.requireNonNull((traversal));
    return new OptionalStringArgumentAndNestedTraversal(string, traversal);
  }
}
