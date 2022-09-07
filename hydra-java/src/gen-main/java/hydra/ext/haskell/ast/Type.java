package hydra.ext.haskell.ast;

public abstract class Type {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Type");
  
  private Type () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Application instance) ;
    
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
  
  public static final class Application extends hydra.ext.haskell.ast.Type {
    public final hydra.ext.haskell.ast.Type_Application value;
    
    public Application (hydra.ext.haskell.ast.Type_Application value) {
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
  
  public static final class Function extends hydra.ext.haskell.ast.Type {
    public final hydra.ext.haskell.ast.Type_Function value;
    
    public Function (hydra.ext.haskell.ast.Type_Function value) {
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
  
  public static final class Infix extends hydra.ext.haskell.ast.Type {
    public final hydra.ext.haskell.ast.Type_Infix value;
    
    public Infix (hydra.ext.haskell.ast.Type_Infix value) {
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
  
  public static final class List extends hydra.ext.haskell.ast.Type {
    public final hydra.ext.haskell.ast.Type value;
    
    public List (hydra.ext.haskell.ast.Type value) {
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
  
  public static final class Parens extends hydra.ext.haskell.ast.Type {
    public final hydra.ext.haskell.ast.Type value;
    
    public Parens (hydra.ext.haskell.ast.Type value) {
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
  
  public static final class Tuple extends hydra.ext.haskell.ast.Type {
    public final java.util.List<hydra.ext.haskell.ast.Type> value;
    
    public Tuple (java.util.List<hydra.ext.haskell.ast.Type> value) {
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
  
  public static final class Variable extends hydra.ext.haskell.ast.Type {
    public final hydra.ext.haskell.ast.Name value;
    
    public Variable (hydra.ext.haskell.ast.Name value) {
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