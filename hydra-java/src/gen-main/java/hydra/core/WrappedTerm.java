// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term wrapped in a type name
 */
public class WrappedTerm implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.WrappedTerm");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  /**
   * The name of the wrapper type
   */
  public final hydra.core.Name typeName;
  
  /**
   * The wrapped term
   */
  public final hydra.core.Term body;
  
  public WrappedTerm (hydra.core.Name typeName, hydra.core.Term body) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((body));
    this.typeName = typeName;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WrappedTerm)) {
      return false;
    }
    WrappedTerm o = (WrappedTerm) (other);
    return typeName.equals(o.typeName) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * body.hashCode();
  }
  
  public WrappedTerm withTypeName(hydra.core.Name typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new WrappedTerm(typeName, body);
  }
  
  public WrappedTerm withBody(hydra.core.Term body) {
    java.util.Objects.requireNonNull((body));
    return new WrappedTerm(typeName, body);
  }
}
