// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * A sequence of term accessors forming a path through a term
 */
public class AccessorPath implements Serializable, Comparable<AccessorPath> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.accessors.AccessorPath");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.accessors.TermAccessor> value;
  
  public AccessorPath (java.util.List<hydra.accessors.TermAccessor> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AccessorPath)) {
      return false;
    }
    AccessorPath o = (AccessorPath) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AccessorPath other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
