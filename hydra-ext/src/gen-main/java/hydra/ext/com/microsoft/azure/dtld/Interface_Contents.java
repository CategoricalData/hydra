// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

public abstract class Interface_Contents implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.Interface.Contents");
  
  public static final hydra.core.Name FIELD_NAME_COMMAND = new hydra.core.Name("command");
  
  public static final hydra.core.Name FIELD_NAME_COMPONENT = new hydra.core.Name("component");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIP = new hydra.core.Name("relationship");
  
  public static final hydra.core.Name FIELD_NAME_TELEMETRY = new hydra.core.Name("telemetry");
  
  private Interface_Contents () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Command instance) ;
    
    R visit(Component instance) ;
    
    R visit(Property instance) ;
    
    R visit(Relationship instance) ;
    
    R visit(Telemetry instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Interface_Contents instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Command instance) {
      return otherwise((instance));
    }
    
    default R visit(Component instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
    
    default R visit(Relationship instance) {
      return otherwise((instance));
    }
    
    default R visit(Telemetry instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Command extends hydra.ext.com.microsoft.azure.dtld.Interface_Contents implements Serializable {
    public final hydra.ext.com.microsoft.azure.dtld.Command value;
    
    public Command (hydra.ext.com.microsoft.azure.dtld.Command value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Command)) {
        return false;
      }
      Command o = (Command) (other);
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
  
  public static final class Component extends hydra.ext.com.microsoft.azure.dtld.Interface_Contents implements Serializable {
    public final hydra.ext.com.microsoft.azure.dtld.Component value;
    
    public Component (hydra.ext.com.microsoft.azure.dtld.Component value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Component)) {
        return false;
      }
      Component o = (Component) (other);
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
  
  public static final class Property extends hydra.ext.com.microsoft.azure.dtld.Interface_Contents implements Serializable {
    public final hydra.ext.com.microsoft.azure.dtld.Property value;
    
    public Property (hydra.ext.com.microsoft.azure.dtld.Property value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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
  
  public static final class Relationship extends hydra.ext.com.microsoft.azure.dtld.Interface_Contents implements Serializable {
    public final hydra.ext.com.microsoft.azure.dtld.Relationship value;
    
    public Relationship (hydra.ext.com.microsoft.azure.dtld.Relationship value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Relationship)) {
        return false;
      }
      Relationship o = (Relationship) (other);
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
  
  public static final class Telemetry extends hydra.ext.com.microsoft.azure.dtld.Interface_Contents implements Serializable {
    public final hydra.ext.com.microsoft.azure.dtld.Telemetry value;
    
    public Telemetry (hydra.ext.com.microsoft.azure.dtld.Telemetry value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Telemetry)) {
        return false;
      }
      Telemetry o = (Telemetry) (other);
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