// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class PredefinedType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PredefinedType");
  
  public static final hydra.core.Name FIELD_NAME_BOOL = new hydra.core.Name("bool");
  
  public static final hydra.core.Name FIELD_NAME_BYTE = new hydra.core.Name("byte");
  
  public static final hydra.core.Name FIELD_NAME_CHAR = new hydra.core.Name("char");
  
  public static final hydra.core.Name FIELD_NAME_DECIMAL = new hydra.core.Name("decimal");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE = new hydra.core.Name("double");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT = new hydra.core.Name("float");
  
  public static final hydra.core.Name FIELD_NAME_INT = new hydra.core.Name("int");
  
  public static final hydra.core.Name FIELD_NAME_LONG = new hydra.core.Name("long");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public static final hydra.core.Name FIELD_NAME_SBYTE = new hydra.core.Name("sbyte");
  
  public static final hydra.core.Name FIELD_NAME_SHORT = new hydra.core.Name("short");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_UINT = new hydra.core.Name("uint");
  
  public static final hydra.core.Name FIELD_NAME_ULONG = new hydra.core.Name("ulong");
  
  public static final hydra.core.Name FIELD_NAME_USHORT = new hydra.core.Name("ushort");
  
  private PredefinedType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Bool instance) ;
    
    R visit(Byte_ instance) ;
    
    R visit(Char instance) ;
    
    R visit(Decimal instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Int instance) ;
    
    R visit(Long_ instance) ;
    
    R visit(Object_ instance) ;
    
    R visit(Sbyte instance) ;
    
    R visit(Short_ instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Uint instance) ;
    
    R visit(Ulong instance) ;
    
    R visit(Ushort instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PredefinedType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Bool instance) {
      return otherwise((instance));
    }
    
    default R visit(Byte_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Char instance) {
      return otherwise((instance));
    }
    
    default R visit(Decimal instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
    
    default R visit(Long_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Sbyte instance) {
      return otherwise((instance));
    }
    
    default R visit(Short_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Uint instance) {
      return otherwise((instance));
    }
    
    default R visit(Ulong instance) {
      return otherwise((instance));
    }
    
    default R visit(Ushort instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Bool extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Bool () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bool)) {
        return false;
      }
      Bool o = (Bool) (other);
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
  
  public static final class Byte_ extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Byte_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Byte_)) {
        return false;
      }
      Byte_ o = (Byte_) (other);
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
  
  public static final class Char extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Char () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Char)) {
        return false;
      }
      Char o = (Char) (other);
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
  
  public static final class Decimal extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Decimal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decimal)) {
        return false;
      }
      Decimal o = (Decimal) (other);
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
  
  public static final class Double_ extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Double_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Double_)) {
        return false;
      }
      Double_ o = (Double_) (other);
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
  
  public static final class Float_ extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Float_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float_)) {
        return false;
      }
      Float_ o = (Float_) (other);
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
  
  public static final class Int extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Int () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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
  
  public static final class Long_ extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Long_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Long_)) {
        return false;
      }
      Long_ o = (Long_) (other);
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
  
  public static final class Object_ extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Object_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) (other);
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
  
  public static final class Sbyte extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Sbyte () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sbyte)) {
        return false;
      }
      Sbyte o = (Sbyte) (other);
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
  
  public static final class Short_ extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Short_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Short_)) {
        return false;
      }
      Short_ o = (Short_) (other);
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
  
  public static final class String_ extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public String_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  public static final class Uint extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Uint () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint)) {
        return false;
      }
      Uint o = (Uint) (other);
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
  
  public static final class Ulong extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Ulong () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ulong)) {
        return false;
      }
      Ulong o = (Ulong) (other);
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
  
  public static final class Ushort extends hydra.ext.csharp.syntax.PredefinedType implements Serializable {
    public Ushort () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ushort)) {
        return false;
      }
      Ushort o = (Ushort) (other);
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