// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralArgumentAndTraversalBiFunctionArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument");
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal;
  
  public final hydra.langs.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction;
  
  public GenericLiteralArgumentAndTraversalBiFunctionArgument (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal, hydra.langs.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction) {
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
  
  public GenericLiteralArgumentAndTraversalBiFunctionArgument withLiteral(hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal) {
    java.util.Objects.requireNonNull((literal));
    return new GenericLiteralArgumentAndTraversalBiFunctionArgument(literal, biFunction);
  }
  
  public GenericLiteralArgumentAndTraversalBiFunctionArgument withBiFunction(hydra.langs.tinkerpop.gremlin.TraversalBiFunctionArgument biFunction) {
    java.util.Objects.requireNonNull((biFunction));
    return new GenericLiteralArgumentAndTraversalBiFunctionArgument(literal, biFunction);
  }
}