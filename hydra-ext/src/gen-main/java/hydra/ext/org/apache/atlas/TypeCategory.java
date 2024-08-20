// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.atlas;

import java.io.Serializable;

public abstract class TypeCategory implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/atlas.TypeCategory");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE = new hydra.core.Name("primitive");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_ID_TYPE = new hydra.core.Name("objectIdType");
  
  public static final hydra.core.Name FIELD_NAME_ENUM = new hydra.core.Name("enum");
  
  public static final hydra.core.Name FIELD_NAME_STRUCT = new hydra.core.Name("struct");
  
  public static final hydra.core.Name FIELD_NAME_CLASSIFICATION = new hydra.core.Name("classification");
  
  public static final hydra.core.Name FIELD_NAME_ENTITY = new hydra.core.Name("entity");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  public static final hydra.core.Name FIELD_NAME_MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIP = new hydra.core.Name("relationship");
  
  public static final hydra.core.Name FIELD_NAME_BUSINESS_METADATA = new hydra.core.Name("businessMetadata");
  
  private TypeCategory () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primitive instance) ;
    
    R visit(ObjectIdType instance) ;
    
    R visit(Enum_ instance) ;
    
    R visit(Struct instance) ;
    
    R visit(Classification instance) ;
    
    R visit(Entity instance) ;
    
    R visit(Array instance) ;
    
    R visit(Map instance) ;
    
    R visit(Relationship instance) ;
    
    R visit(BusinessMetadata instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeCategory instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectIdType instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Struct instance) {
      return otherwise((instance));
    }
    
    default R visit(Classification instance) {
      return otherwise((instance));
    }
    
    default R visit(Entity instance) {
      return otherwise((instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Relationship instance) {
      return otherwise((instance));
    }
    
    default R visit(BusinessMetadata instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primitive extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public Primitive () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) (other);
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
  
  public static final class ObjectIdType extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public ObjectIdType () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectIdType)) {
        return false;
      }
      ObjectIdType o = (ObjectIdType) (other);
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
  
  public static final class Enum_ extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public Enum_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enum_)) {
        return false;
      }
      Enum_ o = (Enum_) (other);
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
  
  public static final class Struct extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public Struct () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Struct)) {
        return false;
      }
      Struct o = (Struct) (other);
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
  
  public static final class Classification extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public Classification () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Classification)) {
        return false;
      }
      Classification o = (Classification) (other);
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
  
  public static final class Entity extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public Entity () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Entity)) {
        return false;
      }
      Entity o = (Entity) (other);
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
  
  public static final class Array extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public Array () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) (other);
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
  
  public static final class Map extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public Map () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
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
  
  public static final class Relationship extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public Relationship () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Relationship)) {
        return false;
      }
      Relationship o = (Relationship) (other);
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
  
  public static final class BusinessMetadata extends hydra.ext.org.apache.atlas.TypeCategory implements Serializable {
    public BusinessMetadata () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BusinessMetadata)) {
        return false;
      }
      BusinessMetadata o = (BusinessMetadata) (other);
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