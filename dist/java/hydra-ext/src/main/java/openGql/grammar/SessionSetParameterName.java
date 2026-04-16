// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SessionSetParameterName implements Serializable, Comparable<SessionSetParameterName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SessionSetParameterName");

  public static final hydra.core.Name IF_NOT_EXISTS = new hydra.core.Name("ifNotExists");

  public static final hydra.core.Name PARAMETER = new hydra.core.Name("parameter");

  public final Boolean ifNotExists;

  public final String parameter;

  public SessionSetParameterName (Boolean ifNotExists, String parameter) {
    this.ifNotExists = ifNotExists;
    this.parameter = parameter;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SessionSetParameterName)) {
      return false;
    }
    SessionSetParameterName o = (SessionSetParameterName) other;
    return java.util.Objects.equals(
      this.ifNotExists,
      o.ifNotExists) && java.util.Objects.equals(
      this.parameter,
      o.parameter);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ifNotExists) + 3 * java.util.Objects.hashCode(parameter);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SessionSetParameterName other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      ifNotExists,
      other.ifNotExists);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      parameter,
      other.parameter);
  }

  public SessionSetParameterName withIfNotExists(Boolean ifNotExists) {
    return new SessionSetParameterName(ifNotExists, parameter);
  }

  public SessionSetParameterName withParameter(String parameter) {
    return new SessionSetParameterName(ifNotExists, parameter);
  }
}
