// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalCardinalityArgumentAndObjects implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalCardinalityArgumentAndObjects");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgument cardinality;
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> objects;
  
  public TraversalCardinalityArgumentAndObjects (hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgument cardinality, java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> objects) {
    if (cardinality == null) {
      throw new IllegalArgumentException("null value for 'cardinality' argument");
    }
    if (objects == null) {
      throw new IllegalArgumentException("null value for 'objects' argument");
    }
    this.cardinality = cardinality;
    this.objects = objects;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalCardinalityArgumentAndObjects)) {
      return false;
    }
    TraversalCardinalityArgumentAndObjects o = (TraversalCardinalityArgumentAndObjects) (other);
    return cardinality.equals(o.cardinality) && objects.equals(o.objects);
  }
  
  @Override
  public int hashCode() {
    return 2 * cardinality.hashCode() + 3 * objects.hashCode();
  }
  
  public TraversalCardinalityArgumentAndObjects withCardinality(hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgument cardinality) {
    if (cardinality == null) {
      throw new IllegalArgumentException("null value for 'cardinality' argument");
    }
    return new TraversalCardinalityArgumentAndObjects(cardinality, objects);
  }
  
  public TraversalCardinalityArgumentAndObjects withObjects(java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> objects) {
    if (objects == null) {
      throw new IllegalArgumentException("null value for 'objects' argument");
    }
    return new TraversalCardinalityArgumentAndObjects(cardinality, objects);
  }
}