package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public abstract class BadProperty<T, V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.BadProperty");
  
  private BadProperty () {
  
  }
  
  public abstract <R> R accept(Visitor<T, V, R> visitor) ;
  
  public interface Visitor<T, V, R> {
    R visit(UnexpectedKey<T, V> instance) ;
    
    R visit(MissingKey<T, V> instance) ;
    
    R visit(Value<T, V> instance) ;
  }
  
  public interface PartialVisitor<T, V, R> extends Visitor<T, V, R> {
    default R otherwise(BadProperty<T, V> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(UnexpectedKey<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(MissingKey<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(Value<T, V> instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * The property key does not have an associated type
   */
  public static final class UnexpectedKey<T, V> extends hydra.langs.tinkerpop.errors.BadProperty<T, V> implements Serializable {
    /**
     * The property key does not have an associated type
     */
    public final hydra.langs.tinkerpop.propertyGraph.PropertyKey value;
    
    public UnexpectedKey (hydra.langs.tinkerpop.propertyGraph.PropertyKey value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnexpectedKey)) {
        return false;
      }
      UnexpectedKey o = (UnexpectedKey) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A required property is missing
   */
  public static final class MissingKey<T, V> extends hydra.langs.tinkerpop.errors.BadProperty<T, V> implements Serializable {
    /**
     * A required property is missing
     */
    public final hydra.langs.tinkerpop.propertyGraph.PropertyKey value;
    
    public MissingKey (hydra.langs.tinkerpop.propertyGraph.PropertyKey value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MissingKey)) {
        return false;
      }
      MissingKey o = (MissingKey) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A property value is invalid
   */
  public static final class Value<T, V> extends hydra.langs.tinkerpop.errors.BadProperty<T, V> implements Serializable {
    /**
     * A property value is invalid
     */
    public final hydra.langs.tinkerpop.errors.TypeError<T, V> value;
    
    public Value (hydra.langs.tinkerpop.errors.TypeError<T, V> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
}