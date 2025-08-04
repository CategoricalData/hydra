// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term.
 */
public class ForallType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.ForallType");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  /**
   * The variable which is bound by the lambda
   */
  public final hydra.core.Name parameter;
  
  /**
   * The body of the lambda
   */
  public final hydra.core.Type body;
  
  public ForallType (hydra.core.Name parameter, hydra.core.Type body) {
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((body));
    this.parameter = parameter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForallType)) {
      return false;
    }
    ForallType o = (ForallType) (other);
    return parameter.equals(o.parameter) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * body.hashCode();
  }
  
  public ForallType withParameter(hydra.core.Name parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new ForallType(parameter, body);
  }
  
  public ForallType withBody(hydra.core.Type body) {
    java.util.Objects.requireNonNull((body));
    return new ForallType(parameter, body);
  }
}
