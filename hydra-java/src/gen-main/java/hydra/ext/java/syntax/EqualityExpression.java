// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class EqualityExpression implements Serializable, Comparable<EqualityExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.EqualityExpression");
  
  public static final hydra.core.Name FIELD_NAME_UNARY = new hydra.core.Name("unary");
  
  public static final hydra.core.Name FIELD_NAME_EQUAL = new hydra.core.Name("equal");
  
  public static final hydra.core.Name FIELD_NAME_NOT_EQUAL = new hydra.core.Name("notEqual");
  
  private EqualityExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Unary instance) ;
    
    R visit(Equal instance) ;
    
    R visit(NotEqual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EqualityExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Unary instance) {
      return otherwise(instance);
    }
    
    default R visit(Equal instance) {
      return otherwise(instance);
    }
    
    default R visit(NotEqual instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Unary extends hydra.ext.java.syntax.EqualityExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression value;
    
    public Unary (hydra.ext.java.syntax.RelationalExpression value) {
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
    public int compareTo(EqualityExpression other) {
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
  
  public static final class Equal extends hydra.ext.java.syntax.EqualityExpression implements Serializable {
    public final hydra.ext.java.syntax.EqualityExpression_Binary value;
    
    public Equal (hydra.ext.java.syntax.EqualityExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equal)) {
        return false;
      }
      Equal o = (Equal) other;
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
    public int compareTo(EqualityExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Equal o = (Equal) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NotEqual extends hydra.ext.java.syntax.EqualityExpression implements Serializable {
    public final hydra.ext.java.syntax.EqualityExpression_Binary value;
    
    public NotEqual (hydra.ext.java.syntax.EqualityExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEqual)) {
        return false;
      }
      NotEqual o = (NotEqual) other;
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
    public int compareTo(EqualityExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotEqual o = (NotEqual) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
