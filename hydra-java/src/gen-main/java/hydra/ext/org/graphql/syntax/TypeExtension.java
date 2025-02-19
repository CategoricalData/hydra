// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public abstract class TypeExtension implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.TypeExtension");
  
  public static final hydra.core.Name FIELD_NAME_SCALAR = new hydra.core.Name("scalar");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE = new hydra.core.Name("interface");
  
  public static final hydra.core.Name FIELD_NAME_UNION = new hydra.core.Name("union");
  
  public static final hydra.core.Name FIELD_NAME_ENUM = new hydra.core.Name("enum");
  
  public static final hydra.core.Name FIELD_NAME_INPUT_OBJECT = new hydra.core.Name("inputObject");
  
  private TypeExtension () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Scalar instance) ;
    
    R visit(Object_ instance) ;
    
    R visit(Interface instance) ;
    
    R visit(Union instance) ;
    
    R visit(Enum_ instance) ;
    
    R visit(InputObject instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeExtension instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Scalar instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Interface instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
    
    default R visit(InputObject instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Scalar extends hydra.ext.org.graphql.syntax.TypeExtension implements Serializable {
    public final hydra.ext.org.graphql.syntax.ScalarTypeExtension value;
    
    public Scalar (hydra.ext.org.graphql.syntax.ScalarTypeExtension value) {
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
  
  public static final class Object_ extends hydra.ext.org.graphql.syntax.TypeExtension implements Serializable {
    public final hydra.ext.org.graphql.syntax.ObjectTypeExtension value;
    
    public Object_ (hydra.ext.org.graphql.syntax.ObjectTypeExtension value) {
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
  
  public static final class Interface extends hydra.ext.org.graphql.syntax.TypeExtension implements Serializable {
    public final hydra.ext.org.graphql.syntax.InterfaceTypeExtension value;
    
    public Interface (hydra.ext.org.graphql.syntax.InterfaceTypeExtension value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interface)) {
        return false;
      }
      Interface o = (Interface) (other);
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
  
  public static final class Union extends hydra.ext.org.graphql.syntax.TypeExtension implements Serializable {
    public final hydra.ext.org.graphql.syntax.UnionTypeExtension value;
    
    public Union (hydra.ext.org.graphql.syntax.UnionTypeExtension value) {
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
  
  public static final class Enum_ extends hydra.ext.org.graphql.syntax.TypeExtension implements Serializable {
    public final hydra.ext.org.graphql.syntax.EnumTypeExtension value;
    
    public Enum_ (hydra.ext.org.graphql.syntax.EnumTypeExtension value) {
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
  
  public static final class InputObject extends hydra.ext.org.graphql.syntax.TypeExtension implements Serializable {
    public final hydra.ext.org.graphql.syntax.InputObjectTypeExtension value;
    
    public InputObject (hydra.ext.org.graphql.syntax.InputObjectTypeExtension value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InputObject)) {
        return false;
      }
      InputObject o = (InputObject) (other);
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