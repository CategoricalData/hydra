// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class UnaryExpressionNotPlusMinus implements Serializable, Comparable<UnaryExpressionNotPlusMinus> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.UnaryExpressionNotPlusMinus");
  
  public static final hydra.core.Name FIELD_NAME_POSTFIX = new hydra.core.Name("postfix");
  
  public static final hydra.core.Name FIELD_NAME_TILDE = new hydra.core.Name("tilde");
  
  public static final hydra.core.Name FIELD_NAME_NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name FIELD_NAME_CAST = new hydra.core.Name("cast");
  
  private UnaryExpressionNotPlusMinus () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Postfix instance) ;
    
    R visit(Tilde instance) ;
    
    R visit(Not instance) ;
    
    R visit(Cast instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryExpressionNotPlusMinus instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Postfix instance) {
      return otherwise(instance);
    }
    
    default R visit(Tilde instance) {
      return otherwise(instance);
    }
    
    default R visit(Not instance) {
      return otherwise(instance);
    }
    
    default R visit(Cast instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Postfix extends hydra.ext.java.syntax.UnaryExpressionNotPlusMinus implements Serializable {
    public final hydra.ext.java.syntax.PostfixExpression value;
    
    public Postfix (hydra.ext.java.syntax.PostfixExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Postfix)) {
        return false;
      }
      Postfix o = (Postfix) other;
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
    public int compareTo(UnaryExpressionNotPlusMinus other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Postfix o = (Postfix) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Tilde extends hydra.ext.java.syntax.UnaryExpressionNotPlusMinus implements Serializable {
    public final hydra.ext.java.syntax.UnaryExpression value;
    
    public Tilde (hydra.ext.java.syntax.UnaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tilde)) {
        return false;
      }
      Tilde o = (Tilde) other;
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
    public int compareTo(UnaryExpressionNotPlusMinus other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Tilde o = (Tilde) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Not extends hydra.ext.java.syntax.UnaryExpressionNotPlusMinus implements Serializable {
    public final hydra.ext.java.syntax.UnaryExpression value;
    
    public Not (hydra.ext.java.syntax.UnaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) other;
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
    public int compareTo(UnaryExpressionNotPlusMinus other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Not o = (Not) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Cast extends hydra.ext.java.syntax.UnaryExpressionNotPlusMinus implements Serializable {
    public final hydra.ext.java.syntax.CastExpression value;
    
    public Cast (hydra.ext.java.syntax.CastExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cast)) {
        return false;
      }
      Cast o = (Cast) other;
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
    public int compareTo(UnaryExpressionNotPlusMinus other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Cast o = (Cast) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
