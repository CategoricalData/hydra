// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A System F type abstraction term
 */
public class TypeAbstraction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.TypeAbstraction");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  /**
   * The type variable introduced by the abstraction
   */
  public final hydra.core.Name parameter;
  
  /**
   * The body of the abstraction
   */
  public final hydra.core.Term body;
  
  public TypeAbstraction (hydra.core.Name parameter, hydra.core.Term body) {
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((body));
    this.parameter = parameter;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeAbstraction)) {
      return false;
    }
    TypeAbstraction o = (TypeAbstraction) (other);
    return parameter.equals(o.parameter) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * body.hashCode();
  }
  
  public TypeAbstraction withParameter(hydra.core.Name parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new TypeAbstraction(parameter, body);
  }
  
  public TypeAbstraction withBody(hydra.core.Term body) {
    java.util.Objects.requireNonNull((body));
    return new TypeAbstraction(parameter, body);
  }
}