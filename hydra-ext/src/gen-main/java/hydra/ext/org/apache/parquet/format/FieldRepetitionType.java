// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.parquet.format;

import java.io.Serializable;

/**
 * Representation of Schemas
 */
public abstract class FieldRepetitionType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.parquet.format.FieldRepetitionType");
  
  public static final hydra.core.Name FIELD_NAME_REQUIRED = new hydra.core.Name("required");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONAL = new hydra.core.Name("optional");
  
  public static final hydra.core.Name FIELD_NAME_REPEATED = new hydra.core.Name("repeated");
  
  private FieldRepetitionType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Required instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Repeated instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FieldRepetitionType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Required instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Repeated instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * This field is required (can not be null) and each record has exactly 1 value.
   */
  public static final class Required extends hydra.ext.org.apache.parquet.format.FieldRepetitionType implements Serializable {
    public Required () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Required)) {
        return false;
      }
      Required o = (Required) (other);
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
  
  /**
   * The field is optional (can be null) and each record has 0 or 1 values.
   */
  public static final class Optional extends hydra.ext.org.apache.parquet.format.FieldRepetitionType implements Serializable {
    public Optional () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
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
  
  /**
   * The field is repeated and can contain 0 or more values
   */
  public static final class Repeated extends hydra.ext.org.apache.parquet.format.FieldRepetitionType implements Serializable {
    public Repeated () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) (other);
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