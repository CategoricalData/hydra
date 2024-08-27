// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralArgumentAndTraversalBiFunctionArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_BI_FUNCTION = new hydra.core.Name("biFunction");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction;
  
  public GenericLiteralArgumentAndTraversalBiFunctionArgument (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal, hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction) {
    java.util.Objects.requireNonNull((literal));
    java.util.Objects.requireNonNull((biFunction));
    this.literal = literal;
    this.biFunction = biFunction;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralArgumentAndTraversalBiFunctionArgument)) {
      return false;
    }
    GenericLiteralArgumentAndTraversalBiFunctionArgument o = (GenericLiteralArgumentAndTraversalBiFunctionArgument) (other);
    return literal.equals(o.literal) && biFunction.equals(o.biFunction);
  }
  
  @Override
  public int hashCode() {
    return 2 * literal.hashCode() + 3 * biFunction.hashCode();
  }
  
  public GenericLiteralArgumentAndTraversalBiFunctionArgument withLiteral(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal) {
    java.util.Objects.requireNonNull((literal));
    return new GenericLiteralArgumentAndTraversalBiFunctionArgument(literal, biFunction);
  }
  
  public GenericLiteralArgumentAndTraversalBiFunctionArgument withBiFunction(hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction) {
    java.util.Objects.requireNonNull((biFunction));
    return new GenericLiteralArgumentAndTraversalBiFunctionArgument(literal, biFunction);
  }
}