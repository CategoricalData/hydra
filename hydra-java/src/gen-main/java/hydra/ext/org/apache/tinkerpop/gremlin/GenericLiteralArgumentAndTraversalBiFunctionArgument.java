// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralArgumentAndTraversalBiFunctionArgument implements Serializable, Comparable<GenericLiteralArgumentAndTraversalBiFunctionArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name BI_FUNCTION = new hydra.core.Name("biFunction");

  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal;

  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction;

  public GenericLiteralArgumentAndTraversalBiFunctionArgument (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal, hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction) {
    this.literal = literal;
    this.biFunction = biFunction;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralArgumentAndTraversalBiFunctionArgument)) {
      return false;
    }
    GenericLiteralArgumentAndTraversalBiFunctionArgument o = (GenericLiteralArgumentAndTraversalBiFunctionArgument) other;
    return java.util.Objects.equals(
      this.literal,
      o.literal) && java.util.Objects.equals(
      this.biFunction,
      o.biFunction);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(literal) + 3 * java.util.Objects.hashCode(biFunction);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GenericLiteralArgumentAndTraversalBiFunctionArgument other) {
    int cmp = 0;
    cmp = ((Comparable) literal).compareTo(other.literal);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) biFunction).compareTo(other.biFunction);
  }

  public GenericLiteralArgumentAndTraversalBiFunctionArgument withLiteral(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal) {
    return new GenericLiteralArgumentAndTraversalBiFunctionArgument(literal, biFunction);
  }

  public GenericLiteralArgumentAndTraversalBiFunctionArgument withBiFunction(hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction) {
    return new GenericLiteralArgumentAndTraversalBiFunctionArgument(literal, biFunction);
  }
}
