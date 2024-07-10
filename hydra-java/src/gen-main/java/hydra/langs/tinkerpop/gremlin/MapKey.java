// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class MapKey implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.MapKey");
  
  private MapKey () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(String_ instance) ;
    
    R visit(Numeric instance) ;
    
    R visit(TraversalToken instance) ;
    
    R visit(TraversalDirection instance) ;
    
    R visit(Set instance) ;
    
    R visit(Collection instance) ;
    
    R visit(Map instance) ;
    
    R visit(Keyword instance) ;
    
    R visit(Identifier instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MapKey instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Numeric instance) {
      return otherwise((instance));
    }
    
    default R visit(TraversalToken instance) {
      return otherwise((instance));
    }
    
    default R visit(TraversalDirection instance) {
      return otherwise((instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
    
    default R visit(Collection instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Keyword instance) {
      return otherwise((instance));
    }
    
    default R visit(Identifier instance) {
      return otherwise((instance));
    }
  }
  
  public static final class String_ extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final String value;
    
    public String_ (String value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  public static final class Numeric extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NumericLiteral value;
    
    public Numeric (hydra.langs.tinkerpop.gremlin.NumericLiteral value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Numeric)) {
        return false;
      }
      Numeric o = (Numeric) (other);
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
  
  public static final class TraversalToken extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalToken value;
    
    public TraversalToken (hydra.langs.tinkerpop.gremlin.TraversalToken value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalToken)) {
        return false;
      }
      TraversalToken o = (TraversalToken) (other);
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
  
  public static final class TraversalDirection extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalDirection value;
    
    public TraversalDirection (hydra.langs.tinkerpop.gremlin.TraversalDirection value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalDirection)) {
        return false;
      }
      TraversalDirection o = (TraversalDirection) (other);
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
  
  public static final class Set extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralSet value;
    
    public Set (hydra.langs.tinkerpop.gremlin.GenericLiteralSet value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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
  
  public static final class Collection extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralCollection value;
    
    public Collection (hydra.langs.tinkerpop.gremlin.GenericLiteralCollection value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Collection)) {
        return false;
      }
      Collection o = (Collection) (other);
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
  
  public static final class Map extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralMap value;
    
    public Map (hydra.langs.tinkerpop.gremlin.GenericLiteralMap value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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
  
  public static final class Keyword extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.Keyword value;
    
    public Keyword (hydra.langs.tinkerpop.gremlin.Keyword value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Keyword)) {
        return false;
      }
      Keyword o = (Keyword) (other);
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
  
  public static final class Identifier extends hydra.langs.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.Identifier value;
    
    public Identifier (hydra.langs.tinkerpop.gremlin.Identifier value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identifier)) {
        return false;
      }
      Identifier o = (Identifier) (other);
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