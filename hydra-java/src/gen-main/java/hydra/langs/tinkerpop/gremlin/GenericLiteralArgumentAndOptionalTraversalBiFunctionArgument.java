// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument");
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalBiFunctionArgument> biFunction;
  
  public GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalBiFunctionArgument> biFunction) {
    java.util.Objects.requireNonNull((literal));
    java.util.Objects.requireNonNull((biFunction));
    this.literal = literal;
    this.biFunction = biFunction;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument)) {
      return false;
    }
    GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument o = (GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument) (other);
    return literal.equals(o.literal) && biFunction.equals(o.biFunction);
  }
  
  @Override
  public int hashCode() {
    return 2 * literal.hashCode() + 3 * biFunction.hashCode();
  }
  
  public GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument withLiteral(hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal) {
    java.util.Objects.requireNonNull((literal));
    return new GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument(literal, biFunction);
  }
  
  public GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument withBiFunction(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalBiFunctionArgument> biFunction) {
    java.util.Objects.requireNonNull((biFunction));
    return new GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument(literal, biFunction);
  }
}