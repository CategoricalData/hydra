// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SessionSetValueParameterClause implements Serializable, Comparable<SessionSetValueParameterClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SessionSetValueParameterClause");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name INITIALIZER = new hydra.core.Name("initializer");

  public final openGql.grammar.SessionSetParameterName value;

  public final openGql.grammar.OptTypedValueInitializer initializer;

  public SessionSetValueParameterClause (openGql.grammar.SessionSetParameterName value, openGql.grammar.OptTypedValueInitializer initializer) {
    this.value = value;
    this.initializer = initializer;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SessionSetValueParameterClause)) {
      return false;
    }
    SessionSetValueParameterClause o = (SessionSetValueParameterClause) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.initializer,
      o.initializer);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(initializer);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SessionSetValueParameterClause other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      initializer,
      other.initializer);
  }

  public SessionSetValueParameterClause withValue(openGql.grammar.SessionSetParameterName value) {
    return new SessionSetValueParameterClause(value, initializer);
  }

  public SessionSetValueParameterClause withInitializer(openGql.grammar.OptTypedValueInitializer initializer) {
    return new SessionSetValueParameterClause(value, initializer);
  }
}
