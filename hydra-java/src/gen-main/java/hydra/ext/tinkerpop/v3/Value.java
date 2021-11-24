package hydra.ext.tinkerpop.v3;

import hydra.core.Literal;

/**
 * A concrete value such as a number or string, a collection of other values, or an element reference
 */
public abstract class Value {
  private Value() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a Value according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Atomic instance) ;
    
    R visit(Collection instance) ;
    
    R visit(Element instance) ;
  }
  
  /**
   * An interface for applying a function to a Value according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Value instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Atomic instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Collection instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Element instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Atomic extends Value {
    public final hydra.core.Literal atomic;
    
    /**
     * Constructs an immutable Atomic object
     */
    public Atomic(hydra.core.Literal atomic) {
      this.atomic = atomic;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Atomic)) {
          return false;
      }
      Atomic o = (Atomic) other;
      return atomic.equals(o.atomic);
    }
    
    @Override
    public int hashCode() {
      return 2 * atomic.hashCode();
    }
  }
  
  public static final class Collection extends Value {
    public final hydra.ext.tinkerpop.v3.CollectionValue collection;
    
    /**
     * Constructs an immutable Collection object
     */
    public Collection(hydra.ext.tinkerpop.v3.CollectionValue collection) {
      this.collection = collection;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Collection)) {
          return false;
      }
      Collection o = (Collection) other;
      return collection.equals(o.collection);
    }
    
    @Override
    public int hashCode() {
      return 2 * collection.hashCode();
    }
  }
  
  public static final class Element extends Value {
    public final hydra.ext.tinkerpop.v3.IdType element;
    
    /**
     * Constructs an immutable Element object
     */
    public Element(hydra.ext.tinkerpop.v3.IdType element) {
      this.element = element;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
          return false;
      }
      Element o = (Element) other;
      return element.equals(o.element);
    }
    
    @Override
    public int hashCode() {
      return 2 * element.hashCode();
    }
  }
}
