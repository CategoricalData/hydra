// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalMergeArgumentAndGenericLiteralMapNullableArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalMergeArgument merge;
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument map;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalCardinality> cardinality;
  
  public TraversalMergeArgumentAndGenericLiteralMapNullableArgument (hydra.langs.tinkerpop.gremlin.TraversalMergeArgument merge, hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument map, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalCardinality> cardinality) {
    java.util.Objects.requireNonNull((merge));
    java.util.Objects.requireNonNull((map));
    java.util.Objects.requireNonNull((cardinality));
    this.merge = merge;
    this.map = map;
    this.cardinality = cardinality;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalMergeArgumentAndGenericLiteralMapNullableArgument)) {
      return false;
    }
    TraversalMergeArgumentAndGenericLiteralMapNullableArgument o = (TraversalMergeArgumentAndGenericLiteralMapNullableArgument) (other);
    return merge.equals(o.merge) && map.equals(o.map) && cardinality.equals(o.cardinality);
  }
  
  @Override
  public int hashCode() {
    return 2 * merge.hashCode() + 3 * map.hashCode() + 5 * cardinality.hashCode();
  }
  
  public TraversalMergeArgumentAndGenericLiteralMapNullableArgument withMerge(hydra.langs.tinkerpop.gremlin.TraversalMergeArgument merge) {
    java.util.Objects.requireNonNull((merge));
    return new TraversalMergeArgumentAndGenericLiteralMapNullableArgument(merge, map, cardinality);
  }
  
  public TraversalMergeArgumentAndGenericLiteralMapNullableArgument withMap(hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument map) {
    java.util.Objects.requireNonNull((map));
    return new TraversalMergeArgumentAndGenericLiteralMapNullableArgument(merge, map, cardinality);
  }
  
  public TraversalMergeArgumentAndGenericLiteralMapNullableArgument withCardinality(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalCardinality> cardinality) {
    java.util.Objects.requireNonNull((cardinality));
    return new TraversalMergeArgumentAndGenericLiteralMapNullableArgument(merge, map, cardinality);
  }
}