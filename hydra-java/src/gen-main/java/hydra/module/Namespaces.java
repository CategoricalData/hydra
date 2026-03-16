// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A mapping from namespaces to values of type n, with a focus on one namespace
 */
public class Namespaces<N> implements Serializable, Comparable<Namespaces<N>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.module.Namespaces");
  
  public static final hydra.core.Name FOCUS = new hydra.core.Name("focus");
  
  public static final hydra.core.Name MAPPING = new hydra.core.Name("mapping");
  
  /**
   * The namespace in focus, together with its associated value
   */
  public final hydra.util.Pair<hydra.module.Namespace, N> focus;
  
  /**
   * A mapping of namespaces to values
   */
  public final hydra.util.PersistentMap<hydra.module.Namespace, N> mapping;
  
  public Namespaces (hydra.util.Pair<hydra.module.Namespace, N> focus, hydra.util.PersistentMap<hydra.module.Namespace, N> mapping) {
    this.focus = focus;
    this.mapping = mapping;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Namespaces)) {
      return false;
    }
    Namespaces o = (Namespaces) other;
    return java.util.Objects.equals(
      this.focus,
      o.focus) && java.util.Objects.equals(
      this.mapping,
      o.mapping);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(focus) + 3 * java.util.Objects.hashCode(mapping);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Namespaces other) {
    int cmp = 0;
    cmp = ((Comparable) focus).compareTo(other.focus);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) mapping).compareTo(other.mapping);
  }
  
  public Namespaces withFocus(hydra.util.Pair<hydra.module.Namespace, N> focus) {
    return new Namespaces(focus, mapping);
  }
  
  public Namespaces withMapping(hydra.util.PersistentMap<hydra.module.Namespace, N> mapping) {
    return new Namespaces(focus, mapping);
  }
}
