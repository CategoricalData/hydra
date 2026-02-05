// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A library of primitive functions
 */
public class Library implements Serializable, Comparable<Library> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.module.Library");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name FIELD_NAME_PREFIX = new hydra.core.Name("prefix");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVES = new hydra.core.Name("primitives");
  
  /**
   * A common prefix for all primitive function names in the library
   */
  public final hydra.module.Namespace namespace;
  
  /**
   * A preferred namespace prefix for function names in the library
   */
  public final String prefix;
  
  /**
   * The primitives defined in this library
   */
  public final java.util.List<hydra.graph.Primitive> primitives;
  
  public Library (hydra.module.Namespace namespace, String prefix, java.util.List<hydra.graph.Primitive> primitives) {
    this.namespace = namespace;
    this.prefix = prefix;
    this.primitives = primitives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Library)) {
      return false;
    }
    Library o = (Library) (other);
    return java.util.Objects.equals(
      this.namespace,
      o.namespace) && java.util.Objects.equals(
      this.prefix,
      o.prefix) && java.util.Objects.equals(
      this.primitives,
      o.primitives);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespace) + 3 * java.util.Objects.hashCode(prefix) + 5 * java.util.Objects.hashCode(primitives);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Library other) {
    int cmp = 0;
    cmp = ((Comparable) (namespace)).compareTo(other.namespace);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (prefix)).compareTo(other.prefix);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      primitives.hashCode(),
      other.primitives.hashCode());
  }
  
  public Library withNamespace(hydra.module.Namespace namespace) {
    return new Library(namespace, prefix, primitives);
  }
  
  public Library withPrefix(String prefix) {
    return new Library(namespace, prefix, primitives);
  }
  
  public Library withPrimitives(java.util.List<hydra.graph.Primitive> primitives) {
    return new Library(namespace, prefix, primitives);
  }
}
