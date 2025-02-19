// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public abstract class NamedType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.avro.schema.NamedType");
  
  public static final hydra.core.Name FIELD_NAME_ENUM = new hydra.core.Name("enum");
  
  public static final hydra.core.Name FIELD_NAME_FIXED = new hydra.core.Name("fixed");
  
  public static final hydra.core.Name FIELD_NAME_RECORD = new hydra.core.Name("record");
  
  private NamedType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Enum_ instance) ;
    
    R visit(Fixed instance) ;
    
    R visit(Record instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NamedType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Fixed instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Enum_ extends hydra.ext.org.apache.avro.schema.NamedType implements Serializable {
    public final hydra.ext.org.apache.avro.schema.Enum_ value;
    
    public Enum_ (hydra.ext.org.apache.avro.schema.Enum_ value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enum_)) {
        return false;
      }
      Enum_ o = (Enum_) (other);
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
  
  public static final class Fixed extends hydra.ext.org.apache.avro.schema.NamedType implements Serializable {
    public final hydra.ext.org.apache.avro.schema.Fixed value;
    
    public Fixed (hydra.ext.org.apache.avro.schema.Fixed value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fixed)) {
        return false;
      }
      Fixed o = (Fixed) (other);
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
  
  public static final class Record extends hydra.ext.org.apache.avro.schema.NamedType implements Serializable {
    public final hydra.ext.org.apache.avro.schema.Record value;
    
    public Record (hydra.ext.org.apache.avro.schema.Record value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) (other);
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