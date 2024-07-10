// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class WhereArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.WhereArgs");
  
  private WhereArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Predicate instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Traversal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WhereArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Predicate instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Traversal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Predicate extends hydra.langs.tinkerpop.gremlin.WhereArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.WhereWithPredicateArgs value;
    
    public Predicate (hydra.langs.tinkerpop.gremlin.WhereWithPredicateArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predicate)) {
        return false;
      }
      Predicate o = (Predicate) (other);
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
  
  public static final class String_ extends hydra.langs.tinkerpop.gremlin.WhereArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgument value;
    
    public String_ (hydra.langs.tinkerpop.gremlin.StringArgument value) {
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
  
  public static final class Traversal extends hydra.langs.tinkerpop.gremlin.WhereArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public Traversal (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Traversal)) {
        return false;
      }
      Traversal o = (Traversal) (other);
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