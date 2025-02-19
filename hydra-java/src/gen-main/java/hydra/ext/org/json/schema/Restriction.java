// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public abstract class Restriction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.Restriction");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER = new hydra.core.Name("number");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public static final hydra.core.Name FIELD_NAME_MULTIPLE = new hydra.core.Name("multiple");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE = new hydra.core.Name("reference");
  
  public static final hydra.core.Name FIELD_NAME_TITLE = new hydra.core.Name("title");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  private Restriction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Type instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Number_ instance) ;
    
    R visit(Array instance) ;
    
    R visit(Object_ instance) ;
    
    R visit(Multiple instance) ;
    
    R visit(Reference instance) ;
    
    R visit(Title instance) ;
    
    R visit(Description instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Restriction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Number_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Multiple instance) {
      return otherwise((instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
    
    default R visit(Title instance) {
      return otherwise((instance));
    }
    
    default R visit(Description instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Type extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final hydra.ext.org.json.schema.Type value;
    
    public Type (hydra.ext.org.json.schema.Type value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class String_ extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final hydra.ext.org.json.schema.StringRestriction value;
    
    public String_ (hydra.ext.org.json.schema.StringRestriction value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  public static final class Number_ extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final hydra.ext.org.json.schema.NumericRestriction value;
    
    public Number_ (hydra.ext.org.json.schema.NumericRestriction value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Number_)) {
        return false;
      }
      Number_ o = (Number_) (other);
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
  
  public static final class Array extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final hydra.ext.org.json.schema.ArrayRestriction value;
    
    public Array (hydra.ext.org.json.schema.ArrayRestriction value) {
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
  
  public static final class Object_ extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final hydra.ext.org.json.schema.ObjectRestriction value;
    
    public Object_ (hydra.ext.org.json.schema.ObjectRestriction value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) (other);
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
  
  public static final class Multiple extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final hydra.ext.org.json.schema.MultipleRestriction value;
    
    public Multiple (hydra.ext.org.json.schema.MultipleRestriction value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Multiple)) {
        return false;
      }
      Multiple o = (Multiple) (other);
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
  
  public static final class Reference extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final hydra.ext.org.json.schema.SchemaReference value;
    
    public Reference (hydra.ext.org.json.schema.SchemaReference value) {
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
  
  public static final class Title extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final String value;
    
    public Title (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Title)) {
        return false;
      }
      Title o = (Title) (other);
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
  
  public static final class Description extends hydra.ext.org.json.schema.Restriction implements Serializable {
    public final String value;
    
    public Description (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Description)) {
        return false;
      }
      Description o = (Description) (other);
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