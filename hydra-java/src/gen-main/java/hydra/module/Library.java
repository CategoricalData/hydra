// Note: this is an automatically generated file. Do not edit.

package hydra.module;

/**
 * A library of primitive functions
 */
public class Library {
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
    java.util.Objects.requireNonNull((namespace));
    java.util.Objects.requireNonNull((prefix));
    java.util.Objects.requireNonNull((primitives));
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
    return namespace.equals(o.namespace) && prefix.equals(o.prefix) && primitives.equals(o.primitives);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * prefix.hashCode() + 5 * primitives.hashCode();
  }
  
  public Library withNamespace(hydra.module.Namespace namespace) {
    java.util.Objects.requireNonNull((namespace));
    return new Library(namespace, prefix, primitives);
  }
  
  public Library withPrefix(String prefix) {
    java.util.Objects.requireNonNull((prefix));
    return new Library(namespace, prefix, primitives);
  }
  
  public Library withPrimitives(java.util.List<hydra.graph.Primitive> primitives) {
    java.util.Objects.requireNonNull((primitives));
    return new Library(namespace, prefix, primitives);
  }
}