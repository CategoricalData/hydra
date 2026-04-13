// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument implements Serializable, Comparable<GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument");

  public static final hydra.core.Name CARDINALITY = new hydra.core.Name("cardinality");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgument cardinality;

  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument object;

  public GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument (hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgument cardinality, hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument object) {
    this.cardinality = cardinality;
    this.object = object;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument)) {
      return false;
    }
    GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument o = (GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument) other;
    return java.util.Objects.equals(
      this.cardinality,
      o.cardinality) && java.util.Objects.equals(
      this.object,
      o.object);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cardinality) + 3 * java.util.Objects.hashCode(object);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      cardinality,
      other.cardinality);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      object,
      other.object);
  }

  public GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument withCardinality(hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgument cardinality) {
    return new GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument(cardinality, object);
  }

  public GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument withObject(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument object) {
    return new GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument(cardinality, object);
  }
}
