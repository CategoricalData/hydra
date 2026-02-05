// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * An abstract expression
 */
public abstract class Expr implements Serializable, Comparable<Expr> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.Expr");
  
  public static final hydra.core.Name FIELD_NAME_CONST = new hydra.core.Name("const");
  
  public static final hydra.core.Name FIELD_NAME_INDENT = new hydra.core.Name("indent");
  
  public static final hydra.core.Name FIELD_NAME_OP = new hydra.core.Name("op");
  
  public static final hydra.core.Name FIELD_NAME_BRACKETS = new hydra.core.Name("brackets");
  
  private Expr () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Const instance) ;
    
    R visit(Indent instance) ;
    
    R visit(Op instance) ;
    
    R visit(Brackets instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Const instance) {
      return otherwise((instance));
    }
    
    default R visit(Indent instance) {
      return otherwise((instance));
    }
    
    default R visit(Op instance) {
      return otherwise((instance));
    }
    
    default R visit(Brackets instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A constant symbol
   */
  public static final class Const extends hydra.ast.Expr implements Serializable {
    public final hydra.ast.Symbol value;
    
    public Const (hydra.ast.Symbol value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Const)) {
        return false;
      }
      Const o = (Const) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Expr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Const o = (Const) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An indented expression
   */
  public static final class Indent extends hydra.ast.Expr implements Serializable {
    public final hydra.ast.IndentedExpression value;
    
    public Indent (hydra.ast.IndentedExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Indent)) {
        return false;
      }
      Indent o = (Indent) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Expr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Indent o = (Indent) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An operator expression
   */
  public static final class Op extends hydra.ast.Expr implements Serializable {
    public final hydra.ast.OpExpr value;
    
    public Op (hydra.ast.OpExpr value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Op)) {
        return false;
      }
      Op o = (Op) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Expr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Op o = (Op) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A bracketed expression
   */
  public static final class Brackets extends hydra.ast.Expr implements Serializable {
    public final hydra.ast.BracketExpr value;
    
    public Brackets (hydra.ast.BracketExpr value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Brackets)) {
        return false;
      }
      Brackets o = (Brackets) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Expr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Brackets o = (Brackets) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
