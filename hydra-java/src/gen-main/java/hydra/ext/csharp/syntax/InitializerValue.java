// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class InitializerValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InitializerValue");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_OR_COLLECTION = new hydra.core.Name("objectOrCollection");
  
  private InitializerValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Expression instance) ;
    
    R visit(ObjectOrCollection instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InitializerValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Expression instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectOrCollection instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Expression extends hydra.ext.csharp.syntax.InitializerValue implements Serializable {
    public final hydra.ext.csharp.syntax.Expression value;
    
    public Expression (hydra.ext.csharp.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expression)) {
        return false;
      }
      Expression o = (Expression) (other);
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
  
  public static final class ObjectOrCollection extends hydra.ext.csharp.syntax.InitializerValue implements Serializable {
    public final hydra.ext.csharp.syntax.ObjectOrCollectionInitializer value;
    
    public ObjectOrCollection (hydra.ext.csharp.syntax.ObjectOrCollectionInitializer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectOrCollection)) {
        return false;
      }
      ObjectOrCollection o = (ObjectOrCollection) (other);
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