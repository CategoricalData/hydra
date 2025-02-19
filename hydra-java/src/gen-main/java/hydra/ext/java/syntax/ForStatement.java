// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ForStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ForStatement");
  
  public static final hydra.core.Name FIELD_NAME_BASIC = new hydra.core.Name("basic");
  
  public static final hydra.core.Name FIELD_NAME_ENHANCED = new hydra.core.Name("enhanced");
  
  private ForStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Basic instance) ;
    
    R visit(Enhanced instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ForStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Basic instance) {
      return otherwise((instance));
    }
    
    default R visit(Enhanced instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Basic extends hydra.ext.java.syntax.ForStatement implements Serializable {
    public final hydra.ext.java.syntax.BasicForStatement value;
    
    public Basic (hydra.ext.java.syntax.BasicForStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Basic)) {
        return false;
      }
      Basic o = (Basic) (other);
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
  
  public static final class Enhanced extends hydra.ext.java.syntax.ForStatement implements Serializable {
    public final hydra.ext.java.syntax.EnhancedForStatement value;
    
    public Enhanced (hydra.ext.java.syntax.EnhancedForStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enhanced)) {
        return false;
      }
      Enhanced o = (Enhanced) (other);
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