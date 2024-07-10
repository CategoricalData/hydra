// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class PropertyArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.PropertyArgs");
  
  private PropertyArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(CardinalityObjects instance) ;
    
    R visit(Objects instance) ;
    
    R visit(Object_ instance) ;
    
    R visit(CardinalityObject instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PropertyArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(CardinalityObjects instance) {
      return otherwise((instance));
    }
    
    default R visit(Objects instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
    
    default R visit(CardinalityObject instance) {
      return otherwise((instance));
    }
  }
  
  public static final class CardinalityObjects extends hydra.langs.tinkerpop.gremlin.PropertyArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects value;
    
    public CardinalityObjects (hydra.langs.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CardinalityObjects)) {
        return false;
      }
      CardinalityObjects o = (CardinalityObjects) (other);
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
  
  public static final class Objects extends hydra.langs.tinkerpop.gremlin.PropertyArgs implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public Objects (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Objects)) {
        return false;
      }
      Objects o = (Objects) (other);
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
  
  public static final class Object_ extends hydra.langs.tinkerpop.gremlin.PropertyArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument value;
    
    public Object_ (hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) (other);
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
  
  public static final class CardinalityObject extends hydra.langs.tinkerpop.gremlin.PropertyArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument value;
    
    public CardinalityObject (hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CardinalityObject)) {
        return false;
      }
      CardinalityObject o = (CardinalityObject) (other);
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