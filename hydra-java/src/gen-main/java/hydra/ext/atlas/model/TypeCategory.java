package hydra.ext.atlas.model;

public abstract class TypeCategory {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.TypeCategory");
  
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
  
  public static final class Primitive extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.Primitive");
    
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
  
  public static final class ObjectIdType extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.ObjectIdType");
    
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
  
  public static final class Enum_ extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.Enum");
    
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
  
  public static final class Struct extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.Struct");
    
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
  
  public static final class Classification extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.Classification");
    
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
  
  public static final class Entity extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.Entity");
    
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
  
  public static final class Array extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.Array");
    
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
  
  public static final class Map extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.Map");
    
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
  
  public static final class Relationship extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.Relationship");
    
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
  
  public static final class BusinessMetadata extends hydra.ext.atlas.model.TypeCategory {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.BusinessMetadata");
    
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