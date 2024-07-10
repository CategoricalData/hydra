// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalDT implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalDT");
  
  private TraversalDT () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Second instance) ;
    
    R visit(Minute instance) ;
    
    R visit(Hour instance) ;
    
    R visit(Day instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalDT instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Second instance) {
      return otherwise((instance));
    }
    
    default R visit(Minute instance) {
      return otherwise((instance));
    }
    
    default R visit(Hour instance) {
      return otherwise((instance));
    }
    
    default R visit(Day instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Second extends hydra.langs.tinkerpop.gremlin.TraversalDT implements Serializable {
    public Second () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Second)) {
        return false;
      }
      Second o = (Second) (other);
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
  
  public static final class Minute extends hydra.langs.tinkerpop.gremlin.TraversalDT implements Serializable {
    public Minute () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minute)) {
        return false;
      }
      Minute o = (Minute) (other);
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
  
  public static final class Hour extends hydra.langs.tinkerpop.gremlin.TraversalDT implements Serializable {
    public Hour () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Hour)) {
        return false;
      }
      Hour o = (Hour) (other);
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
  
  public static final class Day extends hydra.langs.tinkerpop.gremlin.TraversalDT implements Serializable {
    public Day () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Day)) {
        return false;
      }
      Day o = (Day) (other);
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