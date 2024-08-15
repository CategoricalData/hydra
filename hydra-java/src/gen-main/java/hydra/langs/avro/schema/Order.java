// Note: this is an automatically generated file. Do not edit.

package hydra.langs.avro.schema;

import java.io.Serializable;

public abstract class Order implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/avro/schema.Order");
  
  public static final hydra.core.Name FIELD_NAME_ASCENDING = new hydra.core.Name("ascending");
  
  public static final hydra.core.Name FIELD_NAME_DESCENDING = new hydra.core.Name("descending");
  
  public static final hydra.core.Name FIELD_NAME_IGNORE = new hydra.core.Name("ignore");
  
  private Order () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ascending instance) ;
    
    R visit(Descending instance) ;
    
    R visit(Ignore instance) ;
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
    
    default R visit(Ignore instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ascending extends hydra.langs.avro.schema.Order implements Serializable {
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
  
  public static final class Descending extends hydra.langs.avro.schema.Order implements Serializable {
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
  
  public static final class Ignore extends hydra.langs.avro.schema.Order implements Serializable {
    public Ignore () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ignore)) {
        return false;
      }
      Ignore o = (Ignore) (other);
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