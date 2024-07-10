// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

/**
 * A built-in function
 */
public class Primitive<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.Primitive");
  
  /**
   * The unique name of the primitive function
   */
  public final hydra.core.Name name;
  
  /**
   * The type signature of the primitive function
   */
  public final hydra.core.Type<A> type;
  
  /**
   * A concrete implementation of the primitive function
   */
  public final java.util.function.Function<java.util.List<hydra.core.Term<A>>, hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Term<A>>> implementation;
  
  public Primitive (hydra.core.Name name, hydra.core.Type<A> type, java.util.function.Function<java.util.List<hydra.core.Term<A>>, hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Term<A>>> implementation) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (implementation == null) {
      throw new IllegalArgumentException("null value for 'implementation' argument");
    }
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
    return name.equals(o.name) && type.equals(o.type) && implementation.equals(o.implementation);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode() + 5 * implementation.hashCode();
  }
  
  public Primitive withName(hydra.core.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Primitive(name, type, implementation);
  }
  
  public Primitive withType(hydra.core.Type<A> type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new Primitive(name, type, implementation);
  }
  
  public Primitive withImplementation(java.util.function.Function<java.util.List<hydra.core.Term<A>>, hydra.compute.Flow<hydra.graph.Graph<A>, hydra.core.Term<A>>> implementation) {
    if (implementation == null) {
      throw new IllegalArgumentException("null value for 'implementation' argument");
    }
    return new Primitive(name, type, implementation);
  }
}