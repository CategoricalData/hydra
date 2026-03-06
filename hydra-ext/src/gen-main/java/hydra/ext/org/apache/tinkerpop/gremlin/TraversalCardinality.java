// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalCardinality implements Serializable, Comparable<TraversalCardinality> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality");
  
  public static final hydra.core.Name SINGLE = new hydra.core.Name("single");
  
  public static final hydra.core.Name SET = new hydra.core.Name("set");
  
  public static final hydra.core.Name LIST = new hydra.core.Name("list");
  
  private TraversalCardinality () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Single instance) ;
    
    R visit(Set instance) ;
    
    R visit(List instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalCardinality instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Single instance) {
      return otherwise(instance);
    }
    
    default R visit(Set instance) {
      return otherwise(instance);
    }
    
    default R visit(List instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Single extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral value;
    
    public Single (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Single)) {
        return false;
      }
      Single o = (Single) other;
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
    public int compareTo(TraversalCardinality other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Single o = (Single) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Set extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral value;
    
    public Set (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral value) {
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
    public int compareTo(TraversalCardinality other) {
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
  
  public static final class List extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral value;
    
    public List (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
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
    public int compareTo(TraversalCardinality other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
