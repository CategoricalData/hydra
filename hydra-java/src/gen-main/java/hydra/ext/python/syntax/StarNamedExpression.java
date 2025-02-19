// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class StarNamedExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.StarNamedExpression");
  
  public static final hydra.core.Name FIELD_NAME_STAR = new hydra.core.Name("star");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  private StarNamedExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Star instance) ;
    
    R visit(Simple instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StarNamedExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Star instance) {
      return otherwise((instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Star extends hydra.ext.python.syntax.StarNamedExpression implements Serializable {
    public final hydra.ext.python.syntax.BitwiseOr value;
    
    public Star (hydra.ext.python.syntax.BitwiseOr value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Star)) {
        return false;
      }
      Star o = (Star) (other);
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
  
  public static final class Simple extends hydra.ext.python.syntax.StarNamedExpression implements Serializable {
    public final hydra.ext.python.syntax.NamedExpression value;
    
    public Simple (hydra.ext.python.syntax.NamedExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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