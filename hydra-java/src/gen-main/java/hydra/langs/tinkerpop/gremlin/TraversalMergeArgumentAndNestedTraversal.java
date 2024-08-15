// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalMergeArgumentAndNestedTraversal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalMergeArgumentAndNestedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_MERGE = new hydra.core.Name("merge");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL = new hydra.core.Name("traversal");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalMergeArgument merge;
  
  public final hydra.langs.tinkerpop.gremlin.NestedTraversal traversal;
  
  public TraversalMergeArgumentAndNestedTraversal (hydra.langs.tinkerpop.gremlin.TraversalMergeArgument merge, hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    java.util.Objects.requireNonNull((merge));
    java.util.Objects.requireNonNull((traversal));
    this.merge = merge;
    this.traversal = traversal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalMergeArgumentAndNestedTraversal)) {
      return false;
    }
    TraversalMergeArgumentAndNestedTraversal o = (TraversalMergeArgumentAndNestedTraversal) (other);
    return merge.equals(o.merge) && traversal.equals(o.traversal);
  }
  
  @Override
  public int hashCode() {
    return 2 * merge.hashCode() + 3 * traversal.hashCode();
  }
  
  public TraversalMergeArgumentAndNestedTraversal withMerge(hydra.langs.tinkerpop.gremlin.TraversalMergeArgument merge) {
    java.util.Objects.requireNonNull((merge));
    return new TraversalMergeArgumentAndNestedTraversal(merge, traversal);
  }
  
  public TraversalMergeArgumentAndNestedTraversal withTraversal(hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    java.util.Objects.requireNonNull((traversal));
    return new TraversalMergeArgumentAndNestedTraversal(merge, traversal);
  }
}