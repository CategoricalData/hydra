// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class StringNullableArgumentAndGenericLiteralArgument implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.StringNullableArgumentAndGenericLiteralArgument");
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument string;
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal;
  
  public StringNullableArgumentAndGenericLiteralArgument (hydra.langs.tinkerpop.gremlin.StringNullableArgument string, hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal) {
    java.util.Objects.requireNonNull((string));
    java.util.Objects.requireNonNull((literal));
    this.string = string;
    this.literal = literal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringNullableArgumentAndGenericLiteralArgument)) {
      return false;
    }
    StringNullableArgumentAndGenericLiteralArgument o = (StringNullableArgumentAndGenericLiteralArgument) (other);
    return string.equals(o.string) && literal.equals(o.literal);
  }
  
  @Override
  public int hashCode() {
    return 2 * string.hashCode() + 3 * literal.hashCode();
  }
  
  public StringNullableArgumentAndGenericLiteralArgument withString(hydra.langs.tinkerpop.gremlin.StringNullableArgument string) {
    java.util.Objects.requireNonNull((string));
    return new StringNullableArgumentAndGenericLiteralArgument(string, literal);
  }
  
  public StringNullableArgumentAndGenericLiteralArgument withLiteral(hydra.langs.tinkerpop.gremlin.GenericLiteralArgument literal) {
    java.util.Objects.requireNonNull((literal));
    return new StringNullableArgumentAndGenericLiteralArgument(string, literal);
  }
}