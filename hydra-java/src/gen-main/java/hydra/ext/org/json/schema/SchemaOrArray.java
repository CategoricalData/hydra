// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public abstract class SchemaOrArray implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.SchemaOrArray");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA = new hydra.core.Name("schema");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  private SchemaOrArray () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Schema instance) ;
    
    R visit(Array instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SchemaOrArray instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Schema instance) {
      return otherwise((instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Schema extends hydra.ext.org.json.schema.SchemaOrArray implements Serializable {
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
  
  public static final class Array extends hydra.ext.org.json.schema.SchemaOrArray implements Serializable {
    public final java.util.List<hydra.ext.org.json.schema.Keyword> value;
    
    public Array (java.util.List<hydra.ext.org.json.schema.Keyword> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) (other);
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