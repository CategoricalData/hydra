// Note: this is an automatically generated file. Do not edit.

package hydra.module;

/**
 * A mapping from namespaces to values of type n, with a focus on one namespace
 */
public class Namespaces<N> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.module.Namespaces");
  
  public static final hydra.core.Name FIELD_NAME_FOCUS = new hydra.core.Name("focus");
  
  public static final hydra.core.Name FIELD_NAME_MAPPING = new hydra.core.Name("mapping");
  
  /**
   * The namespace in focus, together with its associated value
   */
  public final hydra.util.Tuple.Tuple2<hydra.module.Namespace, N> focus;
  
  /**
   * A mapping of namespaces to values
   */
  public final java.util.Map<hydra.module.Namespace, N> mapping;
  
  public Namespaces (hydra.util.Tuple.Tuple2<hydra.module.Namespace, N> focus, java.util.Map<hydra.module.Namespace, N> mapping) {
    java.util.Objects.requireNonNull((focus));
    java.util.Objects.requireNonNull((mapping));
    this.focus = focus;
    this.mapping = mapping;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Namespaces)) {
      return false;
    }
    Namespaces o = (Namespaces) (other);
    return focus.equals(o.focus) && mapping.equals(o.mapping);
  }
  
  @Override
  public int hashCode() {
    return 2 * focus.hashCode() + 3 * mapping.hashCode();
  }
  
  public Namespaces withFocus(hydra.util.Tuple.Tuple2<hydra.module.Namespace, N> focus) {
    java.util.Objects.requireNonNull((focus));
    return new Namespaces(focus, mapping);
  }
  
  public Namespaces withMapping(java.util.Map<hydra.module.Namespace, N> mapping) {
    java.util.Objects.requireNonNull((mapping));
    return new Namespaces(focus, mapping);
  }
}
