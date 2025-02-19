// Note: this is an automatically generated file. Do not edit.

package hydra.ext.typeScript.model;

import java.io.Serializable;

public abstract class Type implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.typeScript.model.Type");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_NEVER = new hydra.core.Name("never");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_LITERAL = new hydra.core.Name("objectLiteral");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE = new hydra.core.Name("primitive");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_UNKNOWN = new hydra.core.Name("unknown");
  
  public static final hydra.core.Name FIELD_NAME_VOID = new hydra.core.Name("void");
  
  private Type () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Array instance) ;
    
    R visit(Function instance) ;
    
    R visit(Never instance) ;
    
    R visit(ObjectLiteral instance) ;
    
    R visit(Primitive instance) ;
    
    R visit(Tuple instance) ;
    
    R visit(Unknown instance) ;
    
    R visit(Void_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
    
    default R visit(Function instance) {
      return otherwise((instance));
    }
    
    default R visit(Never instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectLiteral instance) {
      return otherwise((instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(Unknown instance) {
      return otherwise((instance));
    }
    
    default R visit(Void_ instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * mutable arrays, also written Array&lt;T&gt;
   */
  public static final class Array extends hydra.ext.typeScript.model.Type implements Serializable {
    public final hydra.ext.typeScript.model.Type value;
    
    public Array (hydra.ext.typeScript.model.Type value) {
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
  
  /**
   * functions
   */
  public static final class Function extends hydra.ext.typeScript.model.Type implements Serializable {
    public final hydra.ext.typeScript.model.FunctionType value;
    
    public Function (hydra.ext.typeScript.model.FunctionType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) (other);
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
   * the bottom type
   */
  public static final class Never extends hydra.ext.typeScript.model.Type implements Serializable {
    public Never () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Never)) {
        return false;
      }
      Never o = (Never) (other);
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
   * e.g. { property: Type }
   */
  public static final class ObjectLiteral extends hydra.ext.typeScript.model.Type implements Serializable {
    public ObjectLiteral () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectLiteral)) {
        return false;
      }
      ObjectLiteral o = (ObjectLiteral) (other);
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
   * A primitive type
   */
  public static final class Primitive extends hydra.ext.typeScript.model.Type implements Serializable {
    public final hydra.ext.typeScript.model.PrimitiveType value;
    
    public Primitive (hydra.ext.typeScript.model.PrimitiveType value) {
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
   * tuples, which are fixed-length but mutable
   */
  public static final class Tuple extends hydra.ext.typeScript.model.Type implements Serializable {
    public final java.util.List<hydra.ext.typeScript.model.Type> value;
    
    public Tuple (java.util.List<hydra.ext.typeScript.model.Type> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
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
   * The top type
   */
  public static final class Unknown extends hydra.ext.typeScript.model.Type implements Serializable {
    public Unknown () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unknown)) {
        return false;
      }
      Unknown o = (Unknown) (other);
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
   * for functions with no documented return value
   */
  public static final class Void_ extends hydra.ext.typeScript.model.Type implements Serializable {
    public Void_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Void_)) {
        return false;
      }
      Void_ o = (Void_) (other);
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