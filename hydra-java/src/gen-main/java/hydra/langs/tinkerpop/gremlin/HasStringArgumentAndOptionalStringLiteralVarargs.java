// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class HasStringArgumentAndOptionalStringLiteralVarargs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargs");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.langs.tinkerpop.gremlin.StringNullableArgument string;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest;
  
  public HasStringArgumentAndOptionalStringLiteralVarargs (hydra.langs.tinkerpop.gremlin.StringNullableArgument string, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest) {
    java.util.Objects.requireNonNull((string));
    java.util.Objects.requireNonNull((rest));
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
    java.util.Objects.requireNonNull((string));
    return new HasStringArgumentAndOptionalStringLiteralVarargs(string, rest);
  }
  
  public HasStringArgumentAndOptionalStringLiteralVarargs withRest(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest> rest) {
    java.util.Objects.requireNonNull((rest));
    return new HasStringArgumentAndOptionalStringLiteralVarargs(string, rest);
  }
}