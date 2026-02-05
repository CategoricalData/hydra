// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

import java.io.Serializable;

/**
 * A built-in function
 */
public class Primitive implements Serializable, Comparable<Primitive> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.graph.Primitive");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTATION = new hydra.core.Name("implementation");
  
  /**
   * The unique name of the primitive function
   */
  public final hydra.core.Name name;
  
  /**
   * The type signature of the primitive function
   */
  public final hydra.core.TypeScheme type;
  
  /**
   * A concrete implementation of the primitive function
   */
  public final java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>> implementation;
  
  public Primitive (hydra.core.Name name, hydra.core.TypeScheme type, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>> implementation) {
    this.name = name;
    this.type = type;
    this.implementation = implementation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Primitive)) {
      return false;
    }
    Primitive o = (Primitive) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.implementation,
      o.implementation);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(implementation);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Primitive other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (type)).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      implementation.hashCode(),
      other.implementation.hashCode());
  }
  
  public Primitive withName(hydra.core.Name name) {
    return new Primitive(name, type, implementation);
  }
  
  public Primitive withType(hydra.core.TypeScheme type) {
    return new Primitive(name, type, implementation);
  }
  
  public Primitive withImplementation(java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>> implementation) {
    return new Primitive(name, type, implementation);
  }
}
