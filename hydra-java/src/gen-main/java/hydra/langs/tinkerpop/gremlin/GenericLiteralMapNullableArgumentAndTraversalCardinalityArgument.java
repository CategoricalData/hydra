// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgument cardinality;
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument object;
  
  public GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument (hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgument cardinality, hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument object) {
    if (cardinality == null) {
      throw new IllegalArgumentException("null value for 'cardinality' argument");
    }
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    this.cardinality = cardinality;
    this.object = object;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument)) {
      return false;
    }
    GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument o = (GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument) (other);
    return cardinality.equals(o.cardinality) && object.equals(o.object);
  }
  
  @Override
  public int hashCode() {
    return 2 * cardinality.hashCode() + 3 * object.hashCode();
  }
  
  public GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument withCardinality(hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgument cardinality) {
    if (cardinality == null) {
      throw new IllegalArgumentException("null value for 'cardinality' argument");
    }
    return new GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument(cardinality, object);
  }
  
  public GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument withObject(hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument object) {
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    return new GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument(cardinality, object);
  }
}