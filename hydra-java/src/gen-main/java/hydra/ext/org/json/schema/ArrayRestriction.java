// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public abstract class ArrayRestriction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.ArrayRestriction");
  
  public static final hydra.core.Name FIELD_NAME_ITEMS = new hydra.core.Name("items");
  
  public static final hydra.core.Name FIELD_NAME_ADDITIONAL_ITEMS = new hydra.core.Name("additionalItems");
  
  public static final hydra.core.Name FIELD_NAME_MIN_ITEMS = new hydra.core.Name("minItems");
  
  public static final hydra.core.Name FIELD_NAME_MAX_ITEMS = new hydra.core.Name("maxItems");
  
  public static final hydra.core.Name FIELD_NAME_UNIQUE_ITEMS = new hydra.core.Name("uniqueItems");
  
  private ArrayRestriction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Items instance) ;
    
    R visit(AdditionalItems instance) ;
    
    R visit(MinItems instance) ;
    
    R visit(MaxItems instance) ;
    
    R visit(UniqueItems instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ArrayRestriction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Items instance) {
      return otherwise((instance));
    }
    
    default R visit(AdditionalItems instance) {
      return otherwise((instance));
    }
    
    default R visit(MinItems instance) {
      return otherwise((instance));
    }
    
    default R visit(MaxItems instance) {
      return otherwise((instance));
    }
    
    default R visit(UniqueItems instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Items extends hydra.ext.org.json.schema.ArrayRestriction implements Serializable {
    public final hydra.ext.org.json.schema.Items value;
    
    public Items (hydra.ext.org.json.schema.Items value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Items)) {
        return false;
      }
      Items o = (Items) (other);
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
  
  public static final class AdditionalItems extends hydra.ext.org.json.schema.ArrayRestriction implements Serializable {
    public final hydra.ext.org.json.schema.AdditionalItems value;
    
    public AdditionalItems (hydra.ext.org.json.schema.AdditionalItems value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AdditionalItems)) {
        return false;
      }
      AdditionalItems o = (AdditionalItems) (other);
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
  
  public static final class MinItems extends hydra.ext.org.json.schema.ArrayRestriction implements Serializable {
    public final Integer value;
    
    public MinItems (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinItems)) {
        return false;
      }
      MinItems o = (MinItems) (other);
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
  
  public static final class MaxItems extends hydra.ext.org.json.schema.ArrayRestriction implements Serializable {
    public final Integer value;
    
    public MaxItems (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxItems)) {
        return false;
      }
      MaxItems o = (MaxItems) (other);
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
  
  public static final class UniqueItems extends hydra.ext.org.json.schema.ArrayRestriction implements Serializable {
    public final Boolean value;
    
    public UniqueItems (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UniqueItems)) {
        return false;
      }
      UniqueItems o = (UniqueItems) (other);
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