// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term wrapped in a type name
 */
public class WrappedTerm implements Serializable, Comparable<WrappedTerm> {
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
    this.typeName = typeName;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WrappedTerm)) {
      return false;
    }
    WrappedTerm o = (WrappedTerm) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WrappedTerm other) {
    int cmp = 0;
    cmp = ((Comparable) typeName).compareTo(other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public WrappedTerm withTypeName(hydra.core.Name typeName) {
    return new WrappedTerm(typeName, body);
  }
  
  public WrappedTerm withBody(hydra.core.Term body) {
    return new WrappedTerm(typeName, body);
  }
}
