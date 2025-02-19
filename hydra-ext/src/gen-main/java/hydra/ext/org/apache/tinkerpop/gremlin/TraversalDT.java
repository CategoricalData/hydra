// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalDT implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT");
  
  public static final hydra.core.Name FIELD_NAME_SECOND = new hydra.core.Name("second");
  
  public static final hydra.core.Name FIELD_NAME_MINUTE = new hydra.core.Name("minute");
  
  public static final hydra.core.Name FIELD_NAME_HOUR = new hydra.core.Name("hour");
  
  public static final hydra.core.Name FIELD_NAME_DAY = new hydra.core.Name("day");
  
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
  
  public static final class Second extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT implements Serializable {
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
  
  public static final class Minute extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT implements Serializable {
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
  
  public static final class Hour extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT implements Serializable {
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
  
  public static final class Day extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT implements Serializable {
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