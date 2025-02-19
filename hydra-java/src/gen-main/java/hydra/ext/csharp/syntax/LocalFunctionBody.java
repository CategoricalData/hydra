// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class LocalFunctionBody implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.LocalFunctionBody");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public static final hydra.core.Name FIELD_NAME_NULL_CONDITIONAL_INVOCATION = new hydra.core.Name("nullConditionalInvocation");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  private LocalFunctionBody () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Block instance) ;
    
    R visit(NullConditionalInvocation instance) ;
    
    R visit(Expression instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocalFunctionBody instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Block instance) {
      return otherwise((instance));
    }
    
    default R visit(NullConditionalInvocation instance) {
      return otherwise((instance));
    }
    
    default R visit(Expression instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Block extends hydra.ext.csharp.syntax.LocalFunctionBody implements Serializable {
    public final hydra.ext.csharp.syntax.Block value;
    
    public Block (hydra.ext.csharp.syntax.Block value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) (other);
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
  
  public static final class NullConditionalInvocation extends hydra.ext.csharp.syntax.LocalFunctionBody implements Serializable {
    public final hydra.ext.csharp.syntax.NullConditionalInvocationExpression value;
    
    public NullConditionalInvocation (hydra.ext.csharp.syntax.NullConditionalInvocationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullConditionalInvocation)) {
        return false;
      }
      NullConditionalInvocation o = (NullConditionalInvocation) (other);
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
  
  public static final class Expression extends hydra.ext.csharp.syntax.LocalFunctionBody implements Serializable {
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
}