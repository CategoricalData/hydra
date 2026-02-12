// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class TermOp implements Serializable, Comparable<TermOp> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TermOp");
  
  public static final hydra.core.Name FIELD_NAME_MUL = new hydra.core.Name("mul");
  
  public static final hydra.core.Name FIELD_NAME_DIV = new hydra.core.Name("div");
  
  public static final hydra.core.Name FIELD_NAME_FLOORDIV = new hydra.core.Name("floordiv");
  
  public static final hydra.core.Name FIELD_NAME_MOD = new hydra.core.Name("mod");
  
  public static final hydra.core.Name FIELD_NAME_MATMUL = new hydra.core.Name("matmul");
  
  private TermOp () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Mul instance) ;
    
    R visit(Div instance) ;
    
    R visit(Floordiv instance) ;
    
    R visit(Mod instance) ;
    
    R visit(Matmul instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TermOp instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Mul instance) {
      return otherwise(instance);
    }
    
    default R visit(Div instance) {
      return otherwise(instance);
    }
    
    default R visit(Floordiv instance) {
      return otherwise(instance);
    }
    
    default R visit(Mod instance) {
      return otherwise(instance);
    }
    
    default R visit(Matmul instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Mul extends hydra.ext.python.syntax.TermOp implements Serializable {
    public Mul () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mul)) {
        return false;
      }
      Mul o = (Mul) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TermOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Div extends hydra.ext.python.syntax.TermOp implements Serializable {
    public Div () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Div)) {
        return false;
      }
      Div o = (Div) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TermOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Floordiv extends hydra.ext.python.syntax.TermOp implements Serializable {
    public Floordiv () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Floordiv)) {
        return false;
      }
      Floordiv o = (Floordiv) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TermOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Mod extends hydra.ext.python.syntax.TermOp implements Serializable {
    public Mod () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mod)) {
        return false;
      }
      Mod o = (Mod) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TermOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Matmul extends hydra.ext.python.syntax.TermOp implements Serializable {
    public Matmul () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Matmul)) {
        return false;
      }
      Matmul o = (Matmul) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TermOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
