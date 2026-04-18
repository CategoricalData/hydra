// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SessionSetBindingTableParameterClause implements Serializable, Comparable<SessionSetBindingTableParameterClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SessionSetBindingTableParameterClause");

  public static final hydra.core.Name BINDING = new hydra.core.Name("binding");

  public static final hydra.core.Name PARAM = new hydra.core.Name("param");

  public static final hydra.core.Name INIT = new hydra.core.Name("init");

  public final Boolean binding;

  public final openGql.grammar.SessionSetParameterName param;

  public final openGql.grammar.OptTypedBindingTableInitializer init;

  public SessionSetBindingTableParameterClause (Boolean binding, openGql.grammar.SessionSetParameterName param, openGql.grammar.OptTypedBindingTableInitializer init) {
    this.binding = binding;
    this.param = param;
    this.init = init;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SessionSetBindingTableParameterClause)) {
      return false;
    }
    SessionSetBindingTableParameterClause o = (SessionSetBindingTableParameterClause) other;
    return java.util.Objects.equals(
      this.binding,
      o.binding) && java.util.Objects.equals(
      this.param,
      o.param) && java.util.Objects.equals(
      this.init,
      o.init);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(binding) + 3 * java.util.Objects.hashCode(param) + 5 * java.util.Objects.hashCode(init);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SessionSetBindingTableParameterClause other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      binding,
      other.binding);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      param,
      other.param);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      init,
      other.init);
  }

  public SessionSetBindingTableParameterClause withBinding(Boolean binding) {
    return new SessionSetBindingTableParameterClause(binding, param, init);
  }

  public SessionSetBindingTableParameterClause withParam(openGql.grammar.SessionSetParameterName param) {
    return new SessionSetBindingTableParameterClause(binding, param, init);
  }

  public SessionSetBindingTableParameterClause withInit(openGql.grammar.OptTypedBindingTableInitializer init) {
    return new SessionSetBindingTableParameterClause(binding, param, init);
  }
}
