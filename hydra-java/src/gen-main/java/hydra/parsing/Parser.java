// Note: this is an automatically generated file. Do not edit.

package hydra.parsing;

/**
 * A parser which consumes characters from a string and produces a value
 */
public class Parser<A> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.parsing.Parser");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.function.Function<String, hydra.parsing.ParseResult<A>> value;
  
  public Parser (java.util.function.Function<String, hydra.parsing.ParseResult<A>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Parser)) {
      return false;
    }
    Parser o = (Parser) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
