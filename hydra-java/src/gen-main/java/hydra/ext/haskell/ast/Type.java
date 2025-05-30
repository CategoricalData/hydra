// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public abstract class Type implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.Type");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION = new hydra.core.Name("application");
  
  public static final hydra.core.Name FIELD_NAME_CTX = new hydra.core.Name("ctx");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_INFIX = new hydra.core.Name("infix");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_PARENS = new hydra.core.Name("parens");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  private Type () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Application instance) ;
    
    R visit(Ctx instance) ;
    
    R visit(Function instance) ;
    
    R visit(Infix instance) ;
    
    R visit(List instance) ;
    
    R visit(Parens instance) ;
    
    R visit(Tuple instance) ;
    
    R visit(Variable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Application instance) {
      return otherwise((instance));
    }
    
    default R visit(Ctx instance) {
      return otherwise((instance));
    }
    
    default R visit(Function instance) {
      return otherwise((instance));
    }
    
    default R visit(Infix instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Application extends hydra.ext.haskell.ast.Type implements Serializable {
    public final hydra.ext.haskell.ast.ApplicationType value;
    
    public Application (hydra.ext.haskell.ast.ApplicationType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) (other);
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
  
  public static final class Ctx extends hydra.ext.haskell.ast.Type implements Serializable {
    public final hydra.ext.haskell.ast.ContextType value;
    
    public Ctx (hydra.ext.haskell.ast.ContextType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ctx)) {
        return false;
      }
      Ctx o = (Ctx) (other);
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
  
  public static final class Function extends hydra.ext.haskell.ast.Type implements Serializable {
    public final hydra.ext.haskell.ast.FunctionType value;
    
    public Function (hydra.ext.haskell.ast.FunctionType value) {
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
  
  public static final class Infix extends hydra.ext.haskell.ast.Type implements Serializable {
    public final hydra.ext.haskell.ast.InfixType value;
    
    public Infix (hydra.ext.haskell.ast.InfixType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Infix)) {
        return false;
      }
      Infix o = (Infix) (other);
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
  
  public static final class List extends hydra.ext.haskell.ast.Type implements Serializable {
    public final hydra.ext.haskell.ast.Type value;
    
    public List (hydra.ext.haskell.ast.Type value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Parens extends hydra.ext.haskell.ast.Type implements Serializable {
    public final hydra.ext.haskell.ast.Type value;
    
    public Parens (hydra.ext.haskell.ast.Type value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) (other);
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
  
  public static final class Tuple extends hydra.ext.haskell.ast.Type implements Serializable {
    public final java.util.List<hydra.ext.haskell.ast.Type> value;
    
    public Tuple (java.util.List<hydra.ext.haskell.ast.Type> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
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
  
  public static final class Variable extends hydra.ext.haskell.ast.Type implements Serializable {
    public final hydra.ext.haskell.ast.Name value;
    
    public Variable (hydra.ext.haskell.ast.Name value) {
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
}