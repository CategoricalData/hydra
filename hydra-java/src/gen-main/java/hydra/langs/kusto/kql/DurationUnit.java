// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class DurationUnit implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.DurationUnit");
  
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
  
  public static final class Second extends hydra.langs.kusto.kql.DurationUnit implements Serializable {
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
  
  public static final class Minute extends hydra.langs.kusto.kql.DurationUnit implements Serializable {
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
  
  public static final class Hour extends hydra.langs.kusto.kql.DurationUnit implements Serializable {
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