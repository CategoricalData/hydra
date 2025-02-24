// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ArrayAccess_Variant implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayAccess_Variant");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  private ArrayAccess_Variant () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Name instance) ;
    
    R visit(Primary instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ArrayAccess_Variant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(Primary instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Name extends hydra.ext.java.syntax.ArrayAccess_Variant implements Serializable {
    public final hydra.ext.java.syntax.ExpressionName value;
    
    public Name (hydra.ext.java.syntax.ExpressionName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
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
  
  public static final class Primary extends hydra.ext.java.syntax.ArrayAccess_Variant implements Serializable {
    public final hydra.ext.java.syntax.PrimaryNoNewArray value;
    
    public Primary (hydra.ext.java.syntax.PrimaryNoNewArray value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) (other);
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