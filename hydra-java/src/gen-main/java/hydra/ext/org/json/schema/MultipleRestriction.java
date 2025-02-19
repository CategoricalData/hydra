// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public abstract class MultipleRestriction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.MultipleRestriction");
  
  public static final hydra.core.Name FIELD_NAME_ALL_OF = new hydra.core.Name("allOf");
  
  public static final hydra.core.Name FIELD_NAME_ANY_OF = new hydra.core.Name("anyOf");
  
  public static final hydra.core.Name FIELD_NAME_ONE_OF = new hydra.core.Name("oneOf");
  
  public static final hydra.core.Name FIELD_NAME_NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name FIELD_NAME_ENUM = new hydra.core.Name("enum");
  
  private MultipleRestriction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AllOf instance) ;
    
    R visit(AnyOf instance) ;
    
    R visit(OneOf instance) ;
    
    R visit(Not instance) ;
    
    R visit(Enum_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MultipleRestriction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AllOf instance) {
      return otherwise((instance));
    }
    
    default R visit(AnyOf instance) {
      return otherwise((instance));
    }
    
    default R visit(OneOf instance) {
      return otherwise((instance));
    }
    
    default R visit(Not instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class AllOf extends hydra.ext.org.json.schema.MultipleRestriction implements Serializable {
    public final java.util.List<hydra.ext.org.json.schema.Schema> value;
    
    public AllOf (java.util.List<hydra.ext.org.json.schema.Schema> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AllOf)) {
        return false;
      }
      AllOf o = (AllOf) (other);
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
  
  public static final class AnyOf extends hydra.ext.org.json.schema.MultipleRestriction implements Serializable {
    public final java.util.List<hydra.ext.org.json.schema.Schema> value;
    
    public AnyOf (java.util.List<hydra.ext.org.json.schema.Schema> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnyOf)) {
        return false;
      }
      AnyOf o = (AnyOf) (other);
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
  
  public static final class OneOf extends hydra.ext.org.json.schema.MultipleRestriction implements Serializable {
    public final java.util.List<hydra.ext.org.json.schema.Schema> value;
    
    public OneOf (java.util.List<hydra.ext.org.json.schema.Schema> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OneOf)) {
        return false;
      }
      OneOf o = (OneOf) (other);
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
  
  public static final class Not extends hydra.ext.org.json.schema.MultipleRestriction implements Serializable {
    public final hydra.ext.org.json.schema.Schema value;
    
    public Not (hydra.ext.org.json.schema.Schema value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) (other);
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
  
  public static final class Enum_ extends hydra.ext.org.json.schema.MultipleRestriction implements Serializable {
    public final java.util.List<hydra.json.Value> value;
    
    public Enum_ (java.util.List<hydra.json.Value> value) {
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
}