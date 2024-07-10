// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class StringArgumentAndGenericLiteralArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.StringArgumentAndGenericLiteralArgument");
  
  public final hydra.langs.tinkerpop.gremlin.StringArgument string;
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal;
  
  public StringArgumentAndGenericLiteralArgument (hydra.langs.tinkerpop.gremlin.StringArgument string, hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    if (literal == null) {
      throw new IllegalArgumentException("null value for 'literal' argument");
    }
    this.string = string;
    this.literal = literal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringArgumentAndGenericLiteralArgument)) {
      return false;
    }
    StringArgumentAndGenericLiteralArgument o = (StringArgumentAndGenericLiteralArgument) (other);
    return string.equals(o.string) && literal.equals(o.literal);
  }
  
  @Override
  public int hashCode() {
    return 2 * string.hashCode() + 3 * literal.hashCode();
  }
  
  public StringArgumentAndGenericLiteralArgument withString(hydra.langs.tinkerpop.gremlin.StringArgument string) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    return new StringArgumentAndGenericLiteralArgument(string, literal);
  }
  
  public StringArgumentAndGenericLiteralArgument withLiteral(hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal) {
    if (literal == null) {
      throw new IllegalArgumentException("null value for 'literal' argument");
    }
    return new StringArgumentAndGenericLiteralArgument(string, literal);
  }
}