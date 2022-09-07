package hydra.ext.tinkerpop.typed;

/**
 * A collection of values, such as a list of strings or an optional integer value
 */
public abstract class CollectionValue {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/tinkerpop/typed.CollectionValue");
  
  private CollectionValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(List instance) ;
    
    R visit(Map instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Set instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CollectionValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
  }
  
  public static final class List extends hydra.ext.tinkerpop.typed.CollectionValue {
    public final java.util.List<hydra.ext.tinkerpop.typed.Value> value;
    
    public List (java.util.List<hydra.ext.tinkerpop.typed.Value> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Map extends hydra.ext.tinkerpop.typed.CollectionValue {
    public final java.util.Map<hydra.ext.tinkerpop.typed.Key, hydra.ext.tinkerpop.typed.Value> value;
    
    public Map (java.util.Map<hydra.ext.tinkerpop.typed.Key, hydra.ext.tinkerpop.typed.Value> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Optional extends hydra.ext.tinkerpop.typed.CollectionValue {
    public final java.util.Optional<hydra.ext.tinkerpop.typed.Value> value;
    
    public Optional (java.util.Optional<hydra.ext.tinkerpop.typed.Value> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Set extends hydra.ext.tinkerpop.typed.CollectionValue {
    public final java.util.Set<hydra.ext.tinkerpop.typed.Value> value;
    
    public Set (java.util.Set<hydra.ext.tinkerpop.typed.Value> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}