// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class HasStringArgumentAndOptionalStringLiteralVarargs implements Serializable, Comparable<HasStringArgumentAndOptionalStringLiteralVarargs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs");
  
  public static final hydra.core.Name STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name REST = new hydra.core.Name("rest");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string;
  
  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest;
  
  public HasStringArgumentAndOptionalStringLiteralVarargs (hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string, hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest) {
    this.string = string;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HasStringArgumentAndOptionalStringLiteralVarargs)) {
      return false;
    }
    HasStringArgumentAndOptionalStringLiteralVarargs o = (HasStringArgumentAndOptionalStringLiteralVarargs) other;
    return java.util.Objects.equals(
      this.string,
      o.string) && java.util.Objects.equals(
      this.rest,
      o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(string) + 3 * java.util.Objects.hashCode(rest);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(HasStringArgumentAndOptionalStringLiteralVarargs other) {
    int cmp = 0;
    cmp = ((Comparable) string).compareTo(other.string);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rest).compareTo(other.rest);
  }
  
  public HasStringArgumentAndOptionalStringLiteralVarargs withString(hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string) {
    return new HasStringArgumentAndOptionalStringLiteralVarargs(string, rest);
  }
  
  public HasStringArgumentAndOptionalStringLiteralVarargs withRest(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest) {
    return new HasStringArgumentAndOptionalStringLiteralVarargs(string, rest);
  }
}
