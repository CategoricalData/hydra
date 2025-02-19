// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public abstract class ObjectRestriction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.ObjectRestriction");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name FIELD_NAME_ADDITIONAL_PROPERTIES = new hydra.core.Name("additionalProperties");
  
  public static final hydra.core.Name FIELD_NAME_REQUIRED = new hydra.core.Name("required");
  
  public static final hydra.core.Name FIELD_NAME_MIN_PROPERTIES = new hydra.core.Name("minProperties");
  
  public static final hydra.core.Name FIELD_NAME_MAX_PROPERTIES = new hydra.core.Name("maxProperties");
  
  public static final hydra.core.Name FIELD_NAME_DEPENDENCIES = new hydra.core.Name("dependencies");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN_PROPERTIES = new hydra.core.Name("patternProperties");
  
  private ObjectRestriction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Properties instance) ;
    
    R visit(AdditionalProperties instance) ;
    
    R visit(Required instance) ;
    
    R visit(MinProperties instance) ;
    
    R visit(MaxProperties instance) ;
    
    R visit(Dependencies instance) ;
    
    R visit(PatternProperties instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ObjectRestriction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Properties instance) {
      return otherwise((instance));
    }
    
    default R visit(AdditionalProperties instance) {
      return otherwise((instance));
    }
    
    default R visit(Required instance) {
      return otherwise((instance));
    }
    
    default R visit(MinProperties instance) {
      return otherwise((instance));
    }
    
    default R visit(MaxProperties instance) {
      return otherwise((instance));
    }
    
    default R visit(Dependencies instance) {
      return otherwise((instance));
    }
    
    default R visit(PatternProperties instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Properties extends hydra.ext.org.json.schema.ObjectRestriction implements Serializable {
    public final java.util.Map<hydra.ext.org.json.schema.Keyword, hydra.ext.org.json.schema.Schema> value;
    
    public Properties (java.util.Map<hydra.ext.org.json.schema.Keyword, hydra.ext.org.json.schema.Schema> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Properties)) {
        return false;
      }
      Properties o = (Properties) (other);
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
  
  public static final class AdditionalProperties extends hydra.ext.org.json.schema.ObjectRestriction implements Serializable {
    public final hydra.ext.org.json.schema.AdditionalItems value;
    
    public AdditionalProperties (hydra.ext.org.json.schema.AdditionalItems value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AdditionalProperties)) {
        return false;
      }
      AdditionalProperties o = (AdditionalProperties) (other);
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
  
  public static final class Required extends hydra.ext.org.json.schema.ObjectRestriction implements Serializable {
    public final java.util.List<hydra.ext.org.json.schema.Keyword> value;
    
    public Required (java.util.List<hydra.ext.org.json.schema.Keyword> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Required)) {
        return false;
      }
      Required o = (Required) (other);
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
  
  public static final class MinProperties extends hydra.ext.org.json.schema.ObjectRestriction implements Serializable {
    public final Integer value;
    
    public MinProperties (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinProperties)) {
        return false;
      }
      MinProperties o = (MinProperties) (other);
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
  
  public static final class MaxProperties extends hydra.ext.org.json.schema.ObjectRestriction implements Serializable {
    public final Integer value;
    
    public MaxProperties (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxProperties)) {
        return false;
      }
      MaxProperties o = (MaxProperties) (other);
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
  
  public static final class Dependencies extends hydra.ext.org.json.schema.ObjectRestriction implements Serializable {
    public final java.util.Map<hydra.ext.org.json.schema.Keyword, hydra.ext.org.json.schema.SchemaOrArray> value;
    
    public Dependencies (java.util.Map<hydra.ext.org.json.schema.Keyword, hydra.ext.org.json.schema.SchemaOrArray> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dependencies)) {
        return false;
      }
      Dependencies o = (Dependencies) (other);
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
  
  public static final class PatternProperties extends hydra.ext.org.json.schema.ObjectRestriction implements Serializable {
    public final java.util.Map<hydra.ext.org.json.schema.RegularExpression, hydra.ext.org.json.schema.Schema> value;
    
    public PatternProperties (java.util.Map<hydra.ext.org.json.schema.RegularExpression, hydra.ext.org.json.schema.Schema> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PatternProperties)) {
        return false;
      }
      PatternProperties o = (PatternProperties) (other);
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