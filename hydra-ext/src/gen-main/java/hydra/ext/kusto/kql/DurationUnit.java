// Note: this is an automatically generated file. Do not edit.

package hydra.ext.kusto.kql;

import java.io.Serializable;

public abstract class DurationUnit implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/kusto/kql.DurationUnit");
  
  public static final hydra.core.Name FIELD_NAME_SECOND = new hydra.core.Name("second");
  
  public static final hydra.core.Name FIELD_NAME_MINUTE = new hydra.core.Name("minute");
  
  public static final hydra.core.Name FIELD_NAME_HOUR = new hydra.core.Name("hour");
  
  private DurationUnit () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Second instance) ;
    
    R visit(Minute instance) ;
    
    R visit(Hour instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DurationUnit instance) {
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
  }
  
  public static final class Second extends hydra.ext.kusto.kql.DurationUnit implements Serializable {
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
  
  public static final class Minute extends hydra.ext.kusto.kql.DurationUnit implements Serializable {
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
  
  public static final class Hour extends hydra.ext.kusto.kql.DurationUnit implements Serializable {
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
}
