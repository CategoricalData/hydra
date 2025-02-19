// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public abstract class Schema implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.avro.schema.Schema");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  public static final hydra.core.Name FIELD_NAME_MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name FIELD_NAME_NAMED = new hydra.core.Name("named");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE = new hydra.core.Name("primitive");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE = new hydra.core.Name("reference");
  
  public static final hydra.core.Name FIELD_NAME_UNION = new hydra.core.Name("union");
  
  private Schema () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Array instance) ;
    
    R visit(Map instance) ;
    
    R visit(Named instance) ;
    
    R visit(Primitive instance) ;
    
    R visit(Reference instance) ;
    
    R visit(Union instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Schema instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Named instance) {
      return otherwise((instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Array extends hydra.ext.org.apache.avro.schema.Schema implements Serializable {
    public final hydra.ext.org.apache.avro.schema.Array value;
    
    public Array (hydra.ext.org.apache.avro.schema.Array value) {
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
  
  public static final class Map extends hydra.ext.org.apache.avro.schema.Schema implements Serializable {
    public final hydra.ext.org.apache.avro.schema.Map value;
    
    public Map (hydra.ext.org.apache.avro.schema.Map value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
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
  
  public static final class Named extends hydra.ext.org.apache.avro.schema.Schema implements Serializable {
    public final hydra.ext.org.apache.avro.schema.Named value;
    
    public Named (hydra.ext.org.apache.avro.schema.Named value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) (other);
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
  
  public static final class Primitive extends hydra.ext.org.apache.avro.schema.Schema implements Serializable {
    public final hydra.ext.org.apache.avro.schema.Primitive value;
    
    public Primitive (hydra.ext.org.apache.avro.schema.Primitive value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) (other);
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
  
  /**
   * A reference by name to a previously defined type
   */
  public static final class Reference extends hydra.ext.org.apache.avro.schema.Schema implements Serializable {
    public final String value;
    
    public Reference (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reference)) {
        return false;
      }
      Reference o = (Reference) (other);
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
  
  public static final class Union extends hydra.ext.org.apache.avro.schema.Schema implements Serializable {
    public final hydra.ext.org.apache.avro.schema.Union value;
    
    public Union (hydra.ext.org.apache.avro.schema.Union value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
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