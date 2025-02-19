// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ConditionalExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ConditionalExpression");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE_CONDITIONAL = new hydra.core.Name("simpleConditional");
  
  public static final hydra.core.Name FIELD_NAME_REF_CONDITIONAL = new hydra.core.Name("refConditional");
  
  private ConditionalExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(SimpleConditional instance) ;
    
    R visit(RefConditional instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConditionalExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
    
    default R visit(SimpleConditional instance) {
      return otherwise((instance));
    }
    
    default R visit(RefConditional instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Simple extends hydra.ext.csharp.syntax.ConditionalExpression implements Serializable {
    public final hydra.ext.csharp.syntax.NullCoalescingExpression value;
    
    public Simple (hydra.ext.csharp.syntax.NullCoalescingExpression value) {
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
  
  public static final class SimpleConditional extends hydra.ext.csharp.syntax.ConditionalExpression implements Serializable {
    public final hydra.ext.csharp.syntax.SimpleConditionalExpression value;
    
    public SimpleConditional (hydra.ext.csharp.syntax.SimpleConditionalExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SimpleConditional)) {
        return false;
      }
      SimpleConditional o = (SimpleConditional) (other);
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
  
  public static final class RefConditional extends hydra.ext.csharp.syntax.ConditionalExpression implements Serializable {
    public final hydra.ext.csharp.syntax.RefConditionalExpression value;
    
    public RefConditional (hydra.ext.csharp.syntax.RefConditionalExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RefConditional)) {
        return false;
      }
      RefConditional o = (RefConditional) (other);
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