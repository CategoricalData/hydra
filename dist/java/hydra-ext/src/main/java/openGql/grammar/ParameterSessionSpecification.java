// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ParameterSessionSpecification implements Serializable, Comparable<ParameterSessionSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ParameterSessionSpecification");

  public static final hydra.core.Name PARAMETER = new hydra.core.Name("parameter");

  public static final hydra.core.Name SESSION_PARAMETER_SPECIFICATION = new hydra.core.Name("sessionParameterSpecification");

  public final Boolean parameter;

  public final String sessionParameterSpecification;

  public ParameterSessionSpecification (Boolean parameter, String sessionParameterSpecification) {
    this.parameter = parameter;
    this.sessionParameterSpecification = sessionParameterSpecification;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParameterSessionSpecification)) {
      return false;
    }
    ParameterSessionSpecification o = (ParameterSessionSpecification) other;
    return java.util.Objects.equals(
      this.parameter,
      o.parameter) && java.util.Objects.equals(
      this.sessionParameterSpecification,
      o.sessionParameterSpecification);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parameter) + 3 * java.util.Objects.hashCode(sessionParameterSpecification);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParameterSessionSpecification other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parameter,
      other.parameter);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sessionParameterSpecification,
      other.sessionParameterSpecification);
  }

  public ParameterSessionSpecification withParameter(Boolean parameter) {
    return new ParameterSessionSpecification(parameter, sessionParameterSpecification);
  }

  public ParameterSessionSpecification withSessionParameterSpecification(String sessionParameterSpecification) {
    return new ParameterSessionSpecification(parameter, sessionParameterSpecification);
  }
}
