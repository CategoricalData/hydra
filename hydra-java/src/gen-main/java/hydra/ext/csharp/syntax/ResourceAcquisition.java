// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ResourceAcquisition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ResourceAcquisition");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL = new hydra.core.Name("local");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  private ResourceAcquisition () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Local instance) ;
    
    R visit(Expression instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ResourceAcquisition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Local instance) {
      return otherwise((instance));
    }
    
    default R visit(Expression instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Local extends hydra.ext.csharp.syntax.ResourceAcquisition implements Serializable {
    public final hydra.ext.csharp.syntax.LocalVariableDeclaration value;
    
    public Local (hydra.ext.csharp.syntax.LocalVariableDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Local)) {
        return false;
      }
      Local o = (Local) (other);
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
  
  public static final class Expression extends hydra.ext.csharp.syntax.ResourceAcquisition implements Serializable {
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