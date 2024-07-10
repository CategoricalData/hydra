// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalOrder implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalOrder");
  
  private TraversalOrder () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Incr instance) ;
    
    R visit(Decr instance) ;
    
    R visit(Asc instance) ;
    
    R visit(Desc instance) ;
    
    R visit(Shuffle instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalOrder instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Incr instance) {
      return otherwise((instance));
    }
    
    default R visit(Decr instance) {
      return otherwise((instance));
    }
    
    default R visit(Asc instance) {
      return otherwise((instance));
    }
    
    default R visit(Desc instance) {
      return otherwise((instance));
    }
    
    default R visit(Shuffle instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Incr extends hydra.langs.tinkerpop.gremlin.TraversalOrder implements Serializable {
    public Incr () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Incr)) {
        return false;
      }
      Incr o = (Incr) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Decr extends hydra.langs.tinkerpop.gremlin.TraversalOrder implements Serializable {
    public Decr () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decr)) {
        return false;
      }
      Decr o = (Decr) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Asc extends hydra.langs.tinkerpop.gremlin.TraversalOrder implements Serializable {
    public Asc () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Asc)) {
        return false;
      }
      Asc o = (Asc) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Desc extends hydra.langs.tinkerpop.gremlin.TraversalOrder implements Serializable {
    public Desc () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Desc)) {
        return false;
      }
      Desc o = (Desc) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Shuffle extends hydra.langs.tinkerpop.gremlin.TraversalOrder implements Serializable {
    public Shuffle () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Shuffle)) {
        return false;
      }
      Shuffle o = (Shuffle) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}