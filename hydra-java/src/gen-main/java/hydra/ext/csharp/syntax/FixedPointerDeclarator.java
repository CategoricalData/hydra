// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class FixedPointerDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FixedPointerDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE = new hydra.core.Name("reference");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  private FixedPointerDeclarator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Reference instance) ;
    
    R visit(Expression instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FixedPointerDeclarator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
    
    default R visit(Expression instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Reference extends hydra.ext.csharp.syntax.FixedPointerDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.VariableReference value;
    
    public Reference (hydra.ext.csharp.syntax.VariableReference value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reference)) {
        return false;
      }
      Reference o = (Reference) (other);
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
  
  public static final class Expression extends hydra.ext.csharp.syntax.FixedPointerDeclarator implements Serializable {
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