// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class StringArgumentAndGenericLiteralArgument implements Serializable, Comparable<StringArgumentAndGenericLiteralArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public final hydra.tinkerpop.gremlin.StringArgument string;

  public final hydra.tinkerpop.gremlin.GenericLiteralArgument literal;

  public StringArgumentAndGenericLiteralArgument (hydra.tinkerpop.gremlin.StringArgument string, hydra.tinkerpop.gremlin.GenericLiteralArgument literal) {
    this.string = string;
    this.literal = literal;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringArgumentAndGenericLiteralArgument)) {
      return false;
    }
    StringArgumentAndGenericLiteralArgument o = (StringArgumentAndGenericLiteralArgument) other;
    return java.util.Objects.equals(
      this.string,
      o.string) && java.util.Objects.equals(
      this.literal,
      o.literal);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(string) + 3 * java.util.Objects.hashCode(literal);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringArgumentAndGenericLiteralArgument other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      string,
      other.string);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      literal,
      other.literal);
  }

  public StringArgumentAndGenericLiteralArgument withString(hydra.tinkerpop.gremlin.StringArgument string) {
    return new StringArgumentAndGenericLiteralArgument(string, literal);
  }

  public StringArgumentAndGenericLiteralArgument withLiteral(hydra.tinkerpop.gremlin.GenericLiteralArgument literal) {
    return new StringArgumentAndGenericLiteralArgument(string, literal);
  }
}
