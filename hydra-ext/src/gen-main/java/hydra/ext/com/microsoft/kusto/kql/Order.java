// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public abstract class Order implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.Order");
  
  public static final hydra.core.Name FIELD_NAME_ASCENDING = new hydra.core.Name("ascending");
  
  public static final hydra.core.Name FIELD_NAME_DESCENDING = new hydra.core.Name("descending");
  
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
  
  public static final class Ascending extends hydra.ext.com.microsoft.kusto.kql.Order implements Serializable {
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
  
  public static final class Descending extends hydra.ext.com.microsoft.kusto.kql.Order implements Serializable {
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