// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalCardinalityArgumentAndObjects implements Serializable, Comparable<TraversalCardinalityArgumentAndObjects> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects");

  public static final hydra.core.Name CARDINALITY = new hydra.core.Name("cardinality");

  public static final hydra.core.Name OBJECTS = new hydra.core.Name("objects");

  public final hydra.tinkerpop.gremlin.TraversalCardinalityArgument cardinality;

  public final java.util.List<hydra.tinkerpop.gremlin.GenericLiteralArgument> objects;

  public TraversalCardinalityArgumentAndObjects (hydra.tinkerpop.gremlin.TraversalCardinalityArgument cardinality, java.util.List<hydra.tinkerpop.gremlin.GenericLiteralArgument> objects) {
    this.cardinality = cardinality;
    this.objects = objects;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalCardinalityArgumentAndObjects)) {
      return false;
    }
    TraversalCardinalityArgumentAndObjects o = (TraversalCardinalityArgumentAndObjects) other;
    return java.util.Objects.equals(
      this.cardinality,
      o.cardinality) && java.util.Objects.equals(
      this.objects,
      o.objects);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cardinality) + 3 * java.util.Objects.hashCode(objects);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TraversalCardinalityArgumentAndObjects other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      cardinality,
      other.cardinality);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      objects,
      other.objects);
  }

  public TraversalCardinalityArgumentAndObjects withCardinality(hydra.tinkerpop.gremlin.TraversalCardinalityArgument cardinality) {
    return new TraversalCardinalityArgumentAndObjects(cardinality, objects);
  }

  public TraversalCardinalityArgumentAndObjects withObjects(java.util.List<hydra.tinkerpop.gremlin.GenericLiteralArgument> objects) {
    return new TraversalCardinalityArgumentAndObjects(cardinality, objects);
  }
}
