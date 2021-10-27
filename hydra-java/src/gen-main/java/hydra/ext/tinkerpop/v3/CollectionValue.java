package hydra.ext.tinkerpop.v3;

public abstract class CollectionValue {
  private CollectionValue() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a CollectionValue according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(List instance) ;
    
    R visit(Map instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Set instance) ;
  }
  
  /**
   * An interface for applying a function to a CollectionValue according to its variant (subclass). If a visit() method
   * for a particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CollectionValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(List instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Map instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Optional instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Set instance) {
      return otherwise(instance);
    }
  }
  
  public static final class List extends CollectionValue {
    public final java.util.List<hydra.ext.tinkerpop.v3.Value> list;
    
    /**
     * Constructs an immutable List object
     */
    public List(java.util.List<hydra.ext.tinkerpop.v3.Value> list) {
      this.list = list;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
          return false;
      }
      List o = (List) other;
      return list.equals(o.list);
    }
    
    @Override
    public int hashCode() {
      return 2 * list.hashCode();
    }
  }
  
  public static final class Map extends CollectionValue {
    public final java.util.Map<String, hydra.ext.tinkerpop.v3.Value> map;
    
    /**
     * Constructs an immutable Map object
     */
    public Map(java.util.Map<String, hydra.ext.tinkerpop.v3.Value> map) {
      this.map = map;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
          return false;
      }
      Map o = (Map) other;
      return map.equals(o.map);
    }
    
    @Override
    public int hashCode() {
      return 2 * map.hashCode();
    }
  }
  
  public static final class Optional extends CollectionValue {
    public final java.util.Optional<hydra.ext.tinkerpop.v3.Value> optional;
    
    /**
     * Constructs an immutable Optional object
     */
    public Optional(java.util.Optional<hydra.ext.tinkerpop.v3.Value> optional) {
      this.optional = optional;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
          return false;
      }
      Optional o = (Optional) other;
      return optional.equals(o.optional);
    }
    
    @Override
    public int hashCode() {
      return 2 * optional.hashCode();
    }
  }
  
  public static final class Set extends CollectionValue {
    public final java.util.Set<hydra.ext.tinkerpop.v3.Value> set;
    
    /**
     * Constructs an immutable Set object
     */
    public Set(java.util.Set<hydra.ext.tinkerpop.v3.Value> set) {
      this.set = set;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
          return false;
      }
      Set o = (Set) other;
      return set.equals(o.set);
    }
    
    @Override
    public int hashCode() {
      return 2 * set.hashCode();
    }
  }
}
