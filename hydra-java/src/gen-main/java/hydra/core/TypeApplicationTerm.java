// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term applied to a type; a type application
 */
public class TypeApplicationTerm implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.TypeApplicationTerm");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The term being applied to a type
   */
  public final hydra.core.Term body;
  
  /**
   * The type argument
   */
  public final hydra.core.Type type;
  
  public TypeApplicationTerm (hydra.core.Term body, hydra.core.Type type) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((type));
    this.body = body;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeApplicationTerm)) {
      return false;
    }
    TypeApplicationTerm o = (TypeApplicationTerm) (other);
    return body.equals(o.body) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * type.hashCode();
  }
  
  public TypeApplicationTerm withBody(hydra.core.Term body) {
    java.util.Objects.requireNonNull((body));
    return new TypeApplicationTerm(body, type);
  }
  
  public TypeApplicationTerm withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypeApplicationTerm(body, type);
  }
}
