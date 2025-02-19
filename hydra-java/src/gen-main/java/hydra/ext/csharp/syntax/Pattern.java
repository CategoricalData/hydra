// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class Pattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.Pattern");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATION = new hydra.core.Name("declaration");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT = new hydra.core.Name("constant");
  
  public static final hydra.core.Name FIELD_NAME_VAR = new hydra.core.Name("var");
  
  private Pattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Declaration instance) ;
    
    R visit(Constant instance) ;
    
    R visit(Var instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Declaration instance) {
      return otherwise((instance));
    }
    
    default R visit(Constant instance) {
      return otherwise((instance));
    }
    
    default R visit(Var instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Declaration extends hydra.ext.csharp.syntax.Pattern implements Serializable {
    public final hydra.ext.csharp.syntax.DeclarationPattern value;
    
    public Declaration (hydra.ext.csharp.syntax.DeclarationPattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Declaration)) {
        return false;
      }
      Declaration o = (Declaration) (other);
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
  
  public static final class Constant extends hydra.ext.csharp.syntax.Pattern implements Serializable {
    public final hydra.ext.csharp.syntax.Expression value;
    
    public Constant (hydra.ext.csharp.syntax.Expression value) {
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
  
  public static final class Var extends hydra.ext.csharp.syntax.Pattern implements Serializable {
    public final hydra.ext.csharp.syntax.Designation value;
    
    public Var (hydra.ext.csharp.syntax.Designation value) {
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
}