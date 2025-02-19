// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class DeclarationStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DeclarationStatement");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT = new hydra.core.Name("constant");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  private DeclarationStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Variable instance) ;
    
    R visit(Constant instance) ;
    
    R visit(Function instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DeclarationStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
    
    default R visit(Constant instance) {
      return otherwise((instance));
    }
    
    default R visit(Function instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Variable extends hydra.ext.csharp.syntax.DeclarationStatement implements Serializable {
    public final hydra.ext.csharp.syntax.LocalVariableDeclaration value;
    
    public Variable (hydra.ext.csharp.syntax.LocalVariableDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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
  
  public static final class Constant extends hydra.ext.csharp.syntax.DeclarationStatement implements Serializable {
    public final hydra.ext.csharp.syntax.LocalConstantDeclaration value;
    
    public Constant (hydra.ext.csharp.syntax.LocalConstantDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) (other);
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
  
  public static final class Function extends hydra.ext.csharp.syntax.DeclarationStatement implements Serializable {
    public final hydra.ext.csharp.syntax.LocalFunctionDeclaration value;
    
    public Function (hydra.ext.csharp.syntax.LocalFunctionDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) (other);
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