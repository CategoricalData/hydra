// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class StringArgumentAndOptionalStringLiteralVarargs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument first;
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> rest;
  
  public StringArgumentAndOptionalStringLiteralVarargs (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument first, java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> rest) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((rest));
    this.first = first;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringArgumentAndOptionalStringLiteralVarargs)) {
      return false;
    }
    StringArgumentAndOptionalStringLiteralVarargs o = (StringArgumentAndOptionalStringLiteralVarargs) (other);
    return first.equals(o.first) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * rest.hashCode();
  }
  
  public StringArgumentAndOptionalStringLiteralVarargs withFirst(hydra.ext.org.apache.tinkerpop.gremlin.StringArgument first) {
    java.util.Objects.requireNonNull((first));
    return new StringArgumentAndOptionalStringLiteralVarargs(first, rest);
  }
  
  public StringArgumentAndOptionalStringLiteralVarargs withRest(java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> rest) {
    java.util.Objects.requireNonNull((rest));
    return new StringArgumentAndOptionalStringLiteralVarargs(first, rest);
  }
}