// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class StringNullableArgumentAndTraversalPredicate implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.StringNullableArgumentAndTraversalPredicate");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument string;
  
  public final hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate;
  
  public StringNullableArgumentAndTraversalPredicate (hydra.langs.tinkerpop.gremlin.StringNullableArgument string, hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate) {
    java.util.Objects.requireNonNull((string));
    java.util.Objects.requireNonNull((predicate));
    this.string = string;
    this.predicate = predicate;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringNullableArgumentAndTraversalPredicate)) {
      return false;
    }
    StringNullableArgumentAndTraversalPredicate o = (StringNullableArgumentAndTraversalPredicate) (other);
    return string.equals(o.string) && predicate.equals(o.predicate);
  }
  
  @Override
  public int hashCode() {
    return 2 * string.hashCode() + 3 * predicate.hashCode();
  }
  
  public StringNullableArgumentAndTraversalPredicate withString(hydra.langs.tinkerpop.gremlin.StringNullableArgument string) {
    java.util.Objects.requireNonNull((string));
    return new StringNullableArgumentAndTraversalPredicate(string, predicate);
  }
  
  public StringNullableArgumentAndTraversalPredicate withPredicate(hydra.langs.tinkerpop.gremlin.TraversalPredicate predicate) {
    java.util.Objects.requireNonNull((predicate));
    return new StringNullableArgumentAndTraversalPredicate(string, predicate);
  }
}