// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class UnionKind implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.UnionKind");
  
  private UnionKind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Inner instance) ;
    
    R visit(Outer instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnionKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Inner instance) {
      return otherwise((instance));
    }
    
    default R visit(Outer instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Inner extends hydra.langs.kusto.kql.UnionKind implements Serializable {
    public Inner () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inner)) {
        return false;
      }
      Inner o = (Inner) (other);
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
  
  public static final class Outer extends hydra.langs.kusto.kql.UnionKind implements Serializable {
    public Outer () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Outer)) {
        return false;
      }
      Outer o = (Outer) (other);
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