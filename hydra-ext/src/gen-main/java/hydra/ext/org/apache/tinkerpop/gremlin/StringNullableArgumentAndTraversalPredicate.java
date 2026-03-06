// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class StringNullableArgumentAndTraversalPredicate implements Serializable, Comparable<StringNullableArgumentAndTraversalPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate");
  
  public static final hydra.core.Name STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate;
  
  public StringNullableArgumentAndTraversalPredicate (hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string, hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate) {
    this.string = string;
    this.predicate = predicate;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringNullableArgumentAndTraversalPredicate)) {
      return false;
    }
    StringNullableArgumentAndTraversalPredicate o = (StringNullableArgumentAndTraversalPredicate) other;
    return java.util.Objects.equals(
      this.string,
      o.string) && java.util.Objects.equals(
      this.predicate,
      o.predicate);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(string) + 3 * java.util.Objects.hashCode(predicate);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringNullableArgumentAndTraversalPredicate other) {
    int cmp = 0;
    cmp = ((Comparable) string).compareTo(other.string);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) predicate).compareTo(other.predicate);
  }
  
  public StringNullableArgumentAndTraversalPredicate withString(hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string) {
    return new StringNullableArgumentAndTraversalPredicate(string, predicate);
  }
  
  public StringNullableArgumentAndTraversalPredicate withPredicate(hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate predicate) {
    return new StringNullableArgumentAndTraversalPredicate(string, predicate);
  }
}
