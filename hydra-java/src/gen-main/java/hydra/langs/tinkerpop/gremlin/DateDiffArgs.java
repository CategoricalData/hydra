// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class DateDiffArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.DateDiffArgs");
  
  private DateDiffArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Traversal instance) ;
    
    R visit(Date instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DateDiffArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Traversal instance) {
      return otherwise((instance));
    }
    
    default R visit(Date instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Traversal extends hydra.langs.tinkerpop.gremlin.DateDiffArgs implements Serializable {
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
  
  public static final class Date extends hydra.langs.tinkerpop.gremlin.DateDiffArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.DateArgument value;
    
    public Date (hydra.langs.tinkerpop.gremlin.DateArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Date)) {
        return false;
      }
      Date o = (Date) (other);
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