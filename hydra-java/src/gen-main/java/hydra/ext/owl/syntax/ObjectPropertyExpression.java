// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public abstract class ObjectPropertyExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.ObjectPropertyExpression");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public static final hydra.core.Name FIELD_NAME_INVERSE_OBJECT = new hydra.core.Name("inverseObject");
  
  private ObjectPropertyExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Object_ instance) ;
    
    R visit(InverseObject instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ObjectPropertyExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
    
    default R visit(InverseObject instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Object_ extends hydra.ext.owl.syntax.ObjectPropertyExpression implements Serializable {
    public final hydra.ext.owl.syntax.ObjectProperty value;
    
    public Object_ (hydra.ext.owl.syntax.ObjectProperty value) {
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
  
  public static final class InverseObject extends hydra.ext.owl.syntax.ObjectPropertyExpression implements Serializable {
    public final hydra.ext.owl.syntax.InverseObjectProperty value;
    
    public InverseObject (hydra.ext.owl.syntax.InverseObjectProperty value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InverseObject)) {
        return false;
      }
      InverseObject o = (InverseObject) (other);
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
