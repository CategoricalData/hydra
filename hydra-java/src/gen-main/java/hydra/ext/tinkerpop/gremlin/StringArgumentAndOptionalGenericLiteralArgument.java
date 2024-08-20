// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class StringArgumentAndOptionalGenericLiteralArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.StringArgumentAndOptionalGenericLiteralArgument");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public final hydra.ext.tinkerpop.gremlin.StringArgument string;
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.GenericLiteralArgument> literal;
  
  public StringArgumentAndOptionalGenericLiteralArgument (hydra.ext.tinkerpop.gremlin.StringArgument string, hydra.util.Opt<hydra.ext.tinkerpop.gremlin.GenericLiteralArgument> literal) {
    java.util.Objects.requireNonNull((string));
    java.util.Objects.requireNonNull((literal));
    this.string = string;
    this.literal = literal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringArgumentAndOptionalGenericLiteralArgument)) {
      return false;
    }
    StringArgumentAndOptionalGenericLiteralArgument o = (StringArgumentAndOptionalGenericLiteralArgument) (other);
    return string.equals(o.string) && literal.equals(o.literal);
  }
  
  @Override
  public int hashCode() {
    return 2 * string.hashCode() + 3 * literal.hashCode();
  }
  
  public StringArgumentAndOptionalGenericLiteralArgument withString(hydra.ext.tinkerpop.gremlin.StringArgument string) {
    java.util.Objects.requireNonNull((string));
    return new StringArgumentAndOptionalGenericLiteralArgument(string, literal);
  }
  
  public StringArgumentAndOptionalGenericLiteralArgument withLiteral(hydra.util.Opt<hydra.ext.tinkerpop.gremlin.GenericLiteralArgument> literal) {
    java.util.Objects.requireNonNull((literal));
    return new StringArgumentAndOptionalGenericLiteralArgument(string, literal);
  }
}
