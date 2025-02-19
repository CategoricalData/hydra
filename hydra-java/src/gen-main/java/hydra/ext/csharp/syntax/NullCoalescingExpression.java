// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class NullCoalescingExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NullCoalescingExpression");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_BINARY = new hydra.core.Name("binary");
  
  public static final hydra.core.Name FIELD_NAME_THROW = new hydra.core.Name("throw");
  
  private NullCoalescingExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(Binary instance) ;
    
    R visit(Throw instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NullCoalescingExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(Throw instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Simple extends hydra.ext.csharp.syntax.NullCoalescingExpression implements Serializable {
    public final hydra.ext.csharp.syntax.ConditionalOrExpression value;
    
    public Simple (hydra.ext.csharp.syntax.ConditionalOrExpression value) {
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
  
  public static final class Binary extends hydra.ext.csharp.syntax.NullCoalescingExpression implements Serializable {
    public final hydra.ext.csharp.syntax.BinaryNullCoalescingExpression value;
    
    public Binary (hydra.ext.csharp.syntax.BinaryNullCoalescingExpression value) {
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
  
  public static final class Throw extends hydra.ext.csharp.syntax.NullCoalescingExpression implements Serializable {
    public final hydra.ext.csharp.syntax.NullCoalescingExpression value;
    
    public Throw (hydra.ext.csharp.syntax.NullCoalescingExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Throw)) {
        return false;
      }
      Throw o = (Throw) (other);
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