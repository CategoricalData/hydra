// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class MultiplicativeExpression implements Serializable, Comparable<MultiplicativeExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MultiplicativeExpression");
  
  public static final hydra.core.Name FIELD_NAME_UNARY = new hydra.core.Name("unary");
  
  public static final hydra.core.Name FIELD_NAME_TIMES = new hydra.core.Name("times");
  
  public static final hydra.core.Name FIELD_NAME_DIVIDE = new hydra.core.Name("divide");
  
  public static final hydra.core.Name FIELD_NAME_MOD = new hydra.core.Name("mod");
  
  private MultiplicativeExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Unary instance) ;
    
    R visit(Times instance) ;
    
    R visit(Divide instance) ;
    
    R visit(Mod instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MultiplicativeExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Unary instance) {
      return otherwise(instance);
    }
    
    default R visit(Times instance) {
      return otherwise(instance);
    }
    
    default R visit(Divide instance) {
      return otherwise(instance);
    }
    
    default R visit(Mod instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Unary extends hydra.ext.java.syntax.MultiplicativeExpression implements Serializable {
    public final hydra.ext.java.syntax.UnaryExpression value;
    
    public Unary (hydra.ext.java.syntax.UnaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unary)) {
        return false;
      }
      Unary o = (Unary) other;
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
    public int compareTo(MultiplicativeExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unary o = (Unary) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Times extends hydra.ext.java.syntax.MultiplicativeExpression implements Serializable {
    public final hydra.ext.java.syntax.MultiplicativeExpression_Binary value;
    
    public Times (hydra.ext.java.syntax.MultiplicativeExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Times)) {
        return false;
      }
      Times o = (Times) other;
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
    public int compareTo(MultiplicativeExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Times o = (Times) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Divide extends hydra.ext.java.syntax.MultiplicativeExpression implements Serializable {
    public final hydra.ext.java.syntax.MultiplicativeExpression_Binary value;
    
    public Divide (hydra.ext.java.syntax.MultiplicativeExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Divide)) {
        return false;
      }
      Divide o = (Divide) other;
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
    public int compareTo(MultiplicativeExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Divide o = (Divide) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Mod extends hydra.ext.java.syntax.MultiplicativeExpression implements Serializable {
    public final hydra.ext.java.syntax.MultiplicativeExpression_Binary value;
    
    public Mod (hydra.ext.java.syntax.MultiplicativeExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mod)) {
        return false;
      }
      Mod o = (Mod) other;
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
    public int compareTo(MultiplicativeExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Mod o = (Mod) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
