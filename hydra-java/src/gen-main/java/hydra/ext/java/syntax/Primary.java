// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class Primary implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.Primary");
  
  public static final hydra.core.Name FIELD_NAME_NO_NEW_ARRAY = new hydra.core.Name("noNewArray");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY_CREATION = new hydra.core.Name("arrayCreation");
  
  private Primary () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(NoNewArray instance) ;
    
    R visit(ArrayCreation instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Primary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(NoNewArray instance) {
      return otherwise((instance));
    }
    
    default R visit(ArrayCreation instance) {
      return otherwise((instance));
    }
  }
  
  public static final class NoNewArray extends hydra.ext.java.syntax.Primary implements Serializable {
    public final hydra.ext.java.syntax.PrimaryNoNewArray value;
    
    public NoNewArray (hydra.ext.java.syntax.PrimaryNoNewArray value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoNewArray)) {
        return false;
      }
      NoNewArray o = (NoNewArray) (other);
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
  
  public static final class ArrayCreation extends hydra.ext.java.syntax.Primary implements Serializable {
    public final hydra.ext.java.syntax.ArrayCreationExpression value;
    
    public ArrayCreation (hydra.ext.java.syntax.ArrayCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ArrayCreation)) {
        return false;
      }
      ArrayCreation o = (ArrayCreation) (other);
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