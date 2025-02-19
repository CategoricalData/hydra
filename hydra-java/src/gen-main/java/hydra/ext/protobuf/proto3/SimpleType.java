// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

/**
 * A scalar type or a reference to an enum type or message type
 */
public abstract class SimpleType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.proto3.SimpleType");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE = new hydra.core.Name("reference");
  
  public static final hydra.core.Name FIELD_NAME_SCALAR = new hydra.core.Name("scalar");
  
  private SimpleType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Reference instance) ;
    
    R visit(Scalar instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimpleType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
    
    default R visit(Scalar instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Reference extends hydra.ext.protobuf.proto3.SimpleType implements Serializable {
    public final hydra.ext.protobuf.proto3.TypeName value;
    
    public Reference (hydra.ext.protobuf.proto3.TypeName value) {
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
  
  public static final class Scalar extends hydra.ext.protobuf.proto3.SimpleType implements Serializable {
    public final hydra.ext.protobuf.proto3.ScalarType value;
    
    public Scalar (hydra.ext.protobuf.proto3.ScalarType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Scalar)) {
        return false;
      }
      Scalar o = (Scalar) (other);
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