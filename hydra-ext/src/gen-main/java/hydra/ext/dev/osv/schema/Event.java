// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

public abstract class Event implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.dev.osv.schema.Event");
  
  public static final hydra.core.Name FIELD_NAME_INTRODUCED = new hydra.core.Name("introduced");
  
  public static final hydra.core.Name FIELD_NAME_FIXED = new hydra.core.Name("fixed");
  
  public static final hydra.core.Name FIELD_NAME_LAST_AFFECTED = new hydra.core.Name("lastAffected");
  
  public static final hydra.core.Name FIELD_NAME_LIMIT = new hydra.core.Name("limit");
  
  private Event () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Introduced instance) ;
    
    R visit(Fixed instance) ;
    
    R visit(LastAffected instance) ;
    
    R visit(Limit instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Event instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Introduced instance) {
      return otherwise((instance));
    }
    
    default R visit(Fixed instance) {
      return otherwise((instance));
    }
    
    default R visit(LastAffected instance) {
      return otherwise((instance));
    }
    
    default R visit(Limit instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Introduced extends hydra.ext.dev.osv.schema.Event implements Serializable {
    public final hydra.ext.dev.osv.schema.VersionOrZero value;
    
    public Introduced (hydra.ext.dev.osv.schema.VersionOrZero value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Introduced)) {
        return false;
      }
      Introduced o = (Introduced) (other);
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
  
  public static final class Fixed extends hydra.ext.dev.osv.schema.Event implements Serializable {
    public final hydra.ext.dev.osv.schema.Version value;
    
    public Fixed (hydra.ext.dev.osv.schema.Version value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fixed)) {
        return false;
      }
      Fixed o = (Fixed) (other);
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
  
  public static final class LastAffected extends hydra.ext.dev.osv.schema.Event implements Serializable {
    public final hydra.ext.dev.osv.schema.Version value;
    
    public LastAffected (hydra.ext.dev.osv.schema.Version value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LastAffected)) {
        return false;
      }
      LastAffected o = (LastAffected) (other);
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
  
  public static final class Limit extends hydra.ext.dev.osv.schema.Event implements Serializable {
    public final hydra.ext.dev.osv.schema.VersionOrStar value;
    
    public Limit (hydra.ext.dev.osv.schema.VersionOrStar value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Limit)) {
        return false;
      }
      Limit o = (Limit) (other);
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