package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class Order implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Order");
  
  private Order () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ascending instance) ;
    
    R visit(Descending instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Order instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ascending instance) {
      return otherwise((instance));
    }
    
    default R visit(Descending instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ascending extends hydra.langs.kusto.kql.Order implements Serializable {
    public Ascending () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ascending)) {
        return false;
      }
      Ascending o = (Ascending) (other);
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
  
  public static final class Descending extends hydra.langs.kusto.kql.Order implements Serializable {
    public Descending () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Descending)) {
        return false;
      }
      Descending o = (Descending) (other);
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