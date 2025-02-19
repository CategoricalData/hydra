// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class AdditiveExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AdditiveExpression");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_BINARY = new hydra.core.Name("binary");
  
  private AdditiveExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(Binary instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AdditiveExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Simple extends hydra.ext.csharp.syntax.AdditiveExpression implements Serializable {
    public final hydra.ext.csharp.syntax.MultiplicativeExpression value;
    
    public Simple (hydra.ext.csharp.syntax.MultiplicativeExpression value) {
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
  
  public static final class Binary extends hydra.ext.csharp.syntax.AdditiveExpression implements Serializable {
    public final hydra.ext.csharp.syntax.BinaryAdditiveExpression value;
    
    public Binary (hydra.ext.csharp.syntax.BinaryAdditiveExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) (other);
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