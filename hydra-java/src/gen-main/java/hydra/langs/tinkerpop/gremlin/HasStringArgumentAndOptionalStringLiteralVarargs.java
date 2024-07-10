// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class HasStringArgumentAndOptionalStringLiteralVarargs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargs");
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument string;
  
  public final java.util.Optional<hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest;
  
  public HasStringArgumentAndOptionalStringLiteralVarargs (hydra.langs.tinkerpop.gremlin.StringNullableArgument string, java.util.Optional<hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    if (rest == null) {
      throw new IllegalArgumentException("null value for 'rest' argument");
    }
    this.string = string;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HasStringArgumentAndOptionalStringLiteralVarargs)) {
      return false;
    }
    HasStringArgumentAndOptionalStringLiteralVarargs o = (HasStringArgumentAndOptionalStringLiteralVarargs) (other);
    return string.equals(o.string) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * string.hashCode() + 3 * rest.hashCode();
  }
  
  public HasStringArgumentAndOptionalStringLiteralVarargs withString(hydra.langs.tinkerpop.gremlin.StringNullableArgument string) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    return new HasStringArgumentAndOptionalStringLiteralVarargs(string, rest);
  }
  
  public HasStringArgumentAndOptionalStringLiteralVarargs withRest(java.util.Optional<hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest) {
    if (rest == null) {
      throw new IllegalArgumentException("null value for 'rest' argument");
    }
    return new HasStringArgumentAndOptionalStringLiteralVarargs(string, rest);
  }
}