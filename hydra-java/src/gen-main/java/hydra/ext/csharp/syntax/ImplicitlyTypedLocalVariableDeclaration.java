// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ImplicitlyTypedLocalVariableDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ImplicitlyTypedLocalVariableDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_VAR = new hydra.core.Name("var");
  
  public static final hydra.core.Name FIELD_NAME_REF_VAR = new hydra.core.Name("refVar");
  
  private ImplicitlyTypedLocalVariableDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Var instance) ;
    
    R visit(RefVar instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImplicitlyTypedLocalVariableDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Var instance) {
      return otherwise((instance));
    }
    
    default R visit(RefVar instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Var extends hydra.ext.csharp.syntax.ImplicitlyTypedLocalVariableDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.ImplicitlyTypedLocalVariableDeclarator value;
    
    public Var (hydra.ext.csharp.syntax.ImplicitlyTypedLocalVariableDeclarator value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) (other);
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
  
  public static final class RefVar extends hydra.ext.csharp.syntax.ImplicitlyTypedLocalVariableDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.RefVarImplicitlyTypedLocalVariableDeclaration value;
    
    public RefVar (hydra.ext.csharp.syntax.RefVarImplicitlyTypedLocalVariableDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RefVar)) {
        return false;
      }
      RefVar o = (RefVar) (other);
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