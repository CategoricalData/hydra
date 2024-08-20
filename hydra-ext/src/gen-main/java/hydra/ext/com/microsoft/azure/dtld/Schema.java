// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * Schemas are used to describe the on-the-wire or serialized format of the data in a digital twin interface. A full set of primitive data types are provided, along with support for a variety of complex schemas in the forms of Arrays, Enums, Maps, and Objects. Schemas described through digital twin's schema definition language are compatible with popular serialization formats, including JSON, Avro, and Protobuf.
 */
public abstract class Schema implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.Schema");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE = new hydra.core.Name("primitive");
  
  public static final hydra.core.Name FIELD_NAME_COMPLEX = new hydra.core.Name("complex");
  
  private Schema () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primitive instance) ;
    
    R visit(Complex instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Schema instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
    
    default R visit(Complex instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primitive extends hydra.ext.com.microsoft.azure.dtld.Schema implements Serializable {
    public final hydra.ext.com.microsoft.azure.dtld.Schema_Primitive value;
    
    public Primitive (hydra.ext.com.microsoft.azure.dtld.Schema_Primitive value) {
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
  
  public static final class Complex extends hydra.ext.com.microsoft.azure.dtld.Schema implements Serializable {
    public final hydra.ext.com.microsoft.azure.dtld.Schema_Complex value;
    
    public Complex (hydra.ext.com.microsoft.azure.dtld.Schema_Complex value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Complex)) {
        return false;
      }
      Complex o = (Complex) (other);
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