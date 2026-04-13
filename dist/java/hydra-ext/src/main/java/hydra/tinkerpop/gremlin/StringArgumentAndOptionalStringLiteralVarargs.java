// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class StringArgumentAndOptionalStringLiteralVarargs implements Serializable, Comparable<StringArgumentAndOptionalStringLiteralVarargs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs");

  public static final hydra.core.Name FIRST = new hydra.core.Name("first");

  public static final hydra.core.Name REST = new hydra.core.Name("rest");

  public final hydra.tinkerpop.gremlin.StringArgument first;

  public final java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument> rest;

  public StringArgumentAndOptionalStringLiteralVarargs (hydra.tinkerpop.gremlin.StringArgument first, java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument> rest) {
    this.first = first;
    this.rest = rest;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringArgumentAndOptionalStringLiteralVarargs)) {
      return false;
    }
    StringArgumentAndOptionalStringLiteralVarargs o = (StringArgumentAndOptionalStringLiteralVarargs) other;
    return java.util.Objects.equals(
      this.first,
      o.first) && java.util.Objects.equals(
      this.rest,
      o.rest);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(first) + 3 * java.util.Objects.hashCode(rest);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringArgumentAndOptionalStringLiteralVarargs other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      first,
      other.first);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rest,
      other.rest);
  }

  public StringArgumentAndOptionalStringLiteralVarargs withFirst(hydra.tinkerpop.gremlin.StringArgument first) {
    return new StringArgumentAndOptionalStringLiteralVarargs(first, rest);
  }

  public StringArgumentAndOptionalStringLiteralVarargs withRest(java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument> rest) {
    return new StringArgumentAndOptionalStringLiteralVarargs(first, rest);
  }
}
