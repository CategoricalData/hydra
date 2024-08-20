// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class NestedTraversalArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.NestedTraversalArgument");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL1 = new hydra.core.Name("traversal1");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL2 = new hydra.core.Name("traversal2");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL3 = new hydra.core.Name("traversal3");
  
  public final hydra.ext.tinkerpop.gremlin.NestedTraversal traversal1;
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.NestedTraversal> traversal2;
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.NestedTraversal> traversal3;
  
  public NestedTraversalArgument (hydra.ext.tinkerpop.gremlin.NestedTraversal traversal1, hydra.util.Opt<hydra.ext.tinkerpop.gremlin.NestedTraversal> traversal2, hydra.util.Opt<hydra.ext.tinkerpop.gremlin.NestedTraversal> traversal3) {
    java.util.Objects.requireNonNull((traversal1));
    java.util.Objects.requireNonNull((traversal2));
    java.util.Objects.requireNonNull((traversal3));
    this.traversal1 = traversal1;
    this.traversal2 = traversal2;
    this.traversal3 = traversal3;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NestedTraversalArgument)) {
      return false;
    }
    NestedTraversalArgument o = (NestedTraversalArgument) (other);
    return traversal1.equals(o.traversal1) && traversal2.equals(o.traversal2) && traversal3.equals(o.traversal3);
  }
  
  @Override
  public int hashCode() {
    return 2 * traversal1.hashCode() + 3 * traversal2.hashCode() + 5 * traversal3.hashCode();
  }
  
  public NestedTraversalArgument withTraversal1(hydra.ext.tinkerpop.gremlin.NestedTraversal traversal1) {
    java.util.Objects.requireNonNull((traversal1));
    return new NestedTraversalArgument(traversal1, traversal2, traversal3);
  }
  
  public NestedTraversalArgument withTraversal2(hydra.util.Opt<hydra.ext.tinkerpop.gremlin.NestedTraversal> traversal2) {
    java.util.Objects.requireNonNull((traversal2));
    return new NestedTraversalArgument(traversal1, traversal2, traversal3);
  }
  
  public NestedTraversalArgument withTraversal3(hydra.util.Opt<hydra.ext.tinkerpop.gremlin.NestedTraversal> traversal3) {
    java.util.Objects.requireNonNull((traversal3));
    return new NestedTraversalArgument(traversal1, traversal2, traversal3);
  }
}
