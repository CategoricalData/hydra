// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class MapKey implements Serializable, Comparable<MapKey> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.MapKey");
  
  public static final hydra.core.Name STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name NUMERIC = new hydra.core.Name("numeric");
  
  public static final hydra.core.Name TRAVERSAL_TOKEN = new hydra.core.Name("traversalToken");
  
  public static final hydra.core.Name TRAVERSAL_DIRECTION = new hydra.core.Name("traversalDirection");
  
  public static final hydra.core.Name SET = new hydra.core.Name("set");
  
  public static final hydra.core.Name COLLECTION = new hydra.core.Name("collection");
  
  public static final hydra.core.Name MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name KEYWORD = new hydra.core.Name("keyword");
  
  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(String_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Numeric instance) {
      return otherwise(instance);
    }
    
    default R visit(TraversalToken instance) {
      return otherwise(instance);
    }
    
    default R visit(TraversalDirection instance) {
      return otherwise(instance);
    }
    
    default R visit(Set instance) {
      return otherwise(instance);
    }
    
    default R visit(Collection instance) {
      return otherwise(instance);
    }
    
    default R visit(Map instance) {
      return otherwise(instance);
    }
    
    default R visit(Keyword instance) {
      return otherwise(instance);
    }
    
    default R visit(Identifier instance) {
      return otherwise(instance);
    }
  }
  
  public static final class String_ extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final String value;
    
    public String_ (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Numeric extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral value;
    
    public Numeric (hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Numeric)) {
        return false;
      }
      Numeric o = (Numeric) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Numeric o = (Numeric) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TraversalToken extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken value;
    
    public TraversalToken (hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalToken)) {
        return false;
      }
      TraversalToken o = (TraversalToken) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TraversalToken o = (TraversalToken) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TraversalDirection extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirection value;
    
    public TraversalDirection (hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirection value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalDirection)) {
        return false;
      }
      TraversalDirection o = (TraversalDirection) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TraversalDirection o = (TraversalDirection) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Set extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralSet value;
    
    public Set (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralSet value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Set o = (Set) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Collection extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralCollection value;
    
    public Collection (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralCollection value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Collection)) {
        return false;
      }
      Collection o = (Collection) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Collection o = (Collection) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Map extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMap value;
    
    public Map (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMap value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Map o = (Map) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Keyword extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.Keyword value;
    
    public Keyword (hydra.ext.org.apache.tinkerpop.gremlin.Keyword value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Keyword)) {
        return false;
      }
      Keyword o = (Keyword) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Keyword o = (Keyword) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Identifier extends hydra.ext.org.apache.tinkerpop.gremlin.MapKey implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.Identifier value;
    
    public Identifier (hydra.ext.org.apache.tinkerpop.gremlin.Identifier value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identifier)) {
        return false;
      }
      Identifier o = (Identifier) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MapKey other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Identifier o = (Identifier) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
