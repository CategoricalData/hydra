// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public abstract class AdditionalItems implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.AdditionalItems");
  
  public static final hydra.core.Name FIELD_NAME_ANY = new hydra.core.Name("any");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA = new hydra.core.Name("schema");
  
  private AdditionalItems () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Any instance) ;
    
    R visit(Schema instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AdditionalItems instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Any instance) {
      return otherwise((instance));
    }
    
    default R visit(Schema instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Any extends hydra.ext.org.json.schema.AdditionalItems implements Serializable {
    public final Boolean value;
    
    public Any (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Any)) {
        return false;
      }
      Any o = (Any) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Schema extends hydra.ext.org.json.schema.AdditionalItems implements Serializable {
    public final hydra.ext.org.json.schema.Schema value;
    
    public Schema (hydra.ext.org.json.schema.Schema value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Schema)) {
        return false;
      }
      Schema o = (Schema) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}