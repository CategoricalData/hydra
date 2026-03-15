// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalPredicateOrStringLiteralVarargs implements Serializable, Comparable<TraversalPredicateOrStringLiteralVarargs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs");
  
  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name STRING = new hydra.core.Name("string");
  
  private TraversalPredicateOrStringLiteralVarargs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Predicate instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalPredicateOrStringLiteralVarargs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Predicate instance) {
      return otherwise(instance);
    }
    
    default R visit(String_ instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Predicate extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value;
    
    public Predicate (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predicate)) {
        return false;
      }
      Predicate o = (Predicate) other;
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
    public int compareTo(TraversalPredicateOrStringLiteralVarargs other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Predicate o = (Predicate) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class String_ extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs implements Serializable {
    public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public String_ (hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
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
    public int compareTo(TraversalPredicateOrStringLiteralVarargs other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
