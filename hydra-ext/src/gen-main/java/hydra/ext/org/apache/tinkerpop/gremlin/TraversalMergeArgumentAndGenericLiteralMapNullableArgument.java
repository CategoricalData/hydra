// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalMergeArgumentAndGenericLiteralMapNullableArgument implements Serializable, Comparable<TraversalMergeArgumentAndGenericLiteralMapNullableArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument");
  
  public static final hydra.core.Name MERGE = new hydra.core.Name("merge");
  
  public static final hydra.core.Name MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name CARDINALITY = new hydra.core.Name("cardinality");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgument merge;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument map;
  
  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality> cardinality;
  
  public TraversalMergeArgumentAndGenericLiteralMapNullableArgument (hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgument merge, hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument map, hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality> cardinality) {
    this.merge = merge;
    this.map = map;
    this.cardinality = cardinality;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalMergeArgumentAndGenericLiteralMapNullableArgument)) {
      return false;
    }
    TraversalMergeArgumentAndGenericLiteralMapNullableArgument o = (TraversalMergeArgumentAndGenericLiteralMapNullableArgument) other;
    return java.util.Objects.equals(
      this.merge,
      o.merge) && java.util.Objects.equals(
      this.map,
      o.map) && java.util.Objects.equals(
      this.cardinality,
      o.cardinality);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(merge) + 3 * java.util.Objects.hashCode(map) + 5 * java.util.Objects.hashCode(cardinality);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TraversalMergeArgumentAndGenericLiteralMapNullableArgument other) {
    int cmp = 0;
    cmp = ((Comparable) merge).compareTo(other.merge);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) map).compareTo(other.map);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) cardinality).compareTo(other.cardinality);
  }
  
  public TraversalMergeArgumentAndGenericLiteralMapNullableArgument withMerge(hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgument merge) {
    return new TraversalMergeArgumentAndGenericLiteralMapNullableArgument(merge, map, cardinality);
  }
  
  public TraversalMergeArgumentAndGenericLiteralMapNullableArgument withMap(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument map) {
    return new TraversalMergeArgumentAndGenericLiteralMapNullableArgument(merge, map, cardinality);
  }
  
  public TraversalMergeArgumentAndGenericLiteralMapNullableArgument withCardinality(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality> cardinality) {
    return new TraversalMergeArgumentAndGenericLiteralMapNullableArgument(merge, map, cardinality);
  }
}
