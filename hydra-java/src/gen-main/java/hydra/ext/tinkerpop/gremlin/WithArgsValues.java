// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class WithArgsValues implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.WithArgsValues");
  
  public static final hydra.core.Name FIELD_NAME_WITH_OPTIONS = new hydra.core.Name("withOptions");
  
  public static final hydra.core.Name FIELD_NAME_IO = new hydra.core.Name("io");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  private WithArgsValues () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(WithOptions instance) ;
    
    R visit(Io instance) ;
    
    R visit(Object_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WithArgsValues instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(WithOptions instance) {
      return otherwise((instance));
    }
    
    default R visit(Io instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class WithOptions extends hydra.ext.tinkerpop.gremlin.WithArgsValues implements Serializable {
    public final hydra.ext.tinkerpop.gremlin.WithOptionsValues value;
    
    public WithOptions (hydra.ext.tinkerpop.gremlin.WithOptionsValues value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithOptions)) {
        return false;
      }
      WithOptions o = (WithOptions) (other);
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
  
  public static final class Io extends hydra.ext.tinkerpop.gremlin.WithArgsValues implements Serializable {
    public final hydra.ext.tinkerpop.gremlin.IoOptionsValues value;
    
    public Io (hydra.ext.tinkerpop.gremlin.IoOptionsValues value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Io)) {
        return false;
      }
      Io o = (Io) (other);
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
  
  public static final class Object_ extends hydra.ext.tinkerpop.gremlin.WithArgsValues implements Serializable {
    public final hydra.ext.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Object_ (hydra.ext.tinkerpop.gremlin.GenericLiteralArgument value) {
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
}
