// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

public abstract class FieldType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.proto3.FieldType");
  
  public static final hydra.core.Name FIELD_NAME_MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name FIELD_NAME_ONEOF = new hydra.core.Name("oneof");
  
  public static final hydra.core.Name FIELD_NAME_REPEATED = new hydra.core.Name("repeated");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  private FieldType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Map instance) ;
    
    R visit(Oneof instance) ;
    
    R visit(Repeated instance) ;
    
    R visit(Simple instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FieldType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Oneof instance) {
      return otherwise((instance));
    }
    
    default R visit(Repeated instance) {
      return otherwise((instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Map extends hydra.ext.protobuf.proto3.FieldType implements Serializable {
    public final hydra.ext.protobuf.proto3.MapType value;
    
    public Map (hydra.ext.protobuf.proto3.MapType value) {
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
  
  public static final class Oneof extends hydra.ext.protobuf.proto3.FieldType implements Serializable {
    public final java.util.List<hydra.ext.protobuf.proto3.Field> value;
    
    public Oneof (java.util.List<hydra.ext.protobuf.proto3.Field> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Oneof)) {
        return false;
      }
      Oneof o = (Oneof) (other);
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
  
  public static final class Repeated extends hydra.ext.protobuf.proto3.FieldType implements Serializable {
    public final hydra.ext.protobuf.proto3.SimpleType value;
    
    public Repeated (hydra.ext.protobuf.proto3.SimpleType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) (other);
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
  
  public static final class Simple extends hydra.ext.protobuf.proto3.FieldType implements Serializable {
    public final hydra.ext.protobuf.proto3.SimpleType value;
    
    public Simple (hydra.ext.protobuf.proto3.SimpleType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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