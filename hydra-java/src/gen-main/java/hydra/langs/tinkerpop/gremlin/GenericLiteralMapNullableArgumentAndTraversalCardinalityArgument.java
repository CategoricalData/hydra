// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument");
  
  public static final hydra.core.Name FIELD_NAME_CARDINALITY = new hydra.core.Name("cardinality");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgument cardinality;
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument object;
  
  public GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument (hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgument cardinality, hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument object) {
    java.util.Objects.requireNonNull((cardinality));
    java.util.Objects.requireNonNull((object));
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
    java.util.Objects.requireNonNull((cardinality));
    return new GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument(cardinality, object);
  }
  
  public GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument withObject(hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument object) {
    java.util.Objects.requireNonNull((object));
    return new GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument(cardinality, object);
  }
}