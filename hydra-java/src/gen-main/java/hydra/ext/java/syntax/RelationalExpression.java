// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class RelationalExpression implements Serializable, Comparable<RelationalExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.RelationalExpression");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN = new hydra.core.Name("lessThan");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN = new hydra.core.Name("greaterThan");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN_EQUAL = new hydra.core.Name("lessThanEqual");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN_EQUAL = new hydra.core.Name("greaterThanEqual");
  
  public static final hydra.core.Name FIELD_NAME_INSTANCEOF = new hydra.core.Name("instanceof");
  
  private RelationalExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(LessThan instance) ;
    
    R visit(GreaterThan instance) ;
    
    R visit(LessThanEqual instance) ;
    
    R visit(GreaterThanEqual instance) ;
    
    R visit(Instanceof instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RelationalExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Simple instance) {
      return otherwise(instance);
    }
    
    default R visit(LessThan instance) {
      return otherwise(instance);
    }
    
    default R visit(GreaterThan instance) {
      return otherwise(instance);
    }
    
    default R visit(LessThanEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(GreaterThanEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(Instanceof instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Simple extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.ShiftExpression value;
    
    public Simple (hydra.ext.java.syntax.ShiftExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(RelationalExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LessThan extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_LessThan value;
    
    public LessThan (hydra.ext.java.syntax.RelationalExpression_LessThan value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThan)) {
        return false;
      }
      LessThan o = (LessThan) other;
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
    public int compareTo(RelationalExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LessThan o = (LessThan) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GreaterThan extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_GreaterThan value;
    
    public GreaterThan (hydra.ext.java.syntax.RelationalExpression_GreaterThan value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThan)) {
        return false;
      }
      GreaterThan o = (GreaterThan) other;
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
    public int compareTo(RelationalExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GreaterThan o = (GreaterThan) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LessThanEqual extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_LessThanEqual value;
    
    public LessThanEqual (hydra.ext.java.syntax.RelationalExpression_LessThanEqual value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThanEqual)) {
        return false;
      }
      LessThanEqual o = (LessThanEqual) other;
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
    public int compareTo(RelationalExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LessThanEqual o = (LessThanEqual) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GreaterThanEqual extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual value;
    
    public GreaterThanEqual (hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThanEqual)) {
        return false;
      }
      GreaterThanEqual o = (GreaterThanEqual) other;
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
    public int compareTo(RelationalExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GreaterThanEqual o = (GreaterThanEqual) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Instanceof extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_InstanceOf value;
    
    public Instanceof (hydra.ext.java.syntax.RelationalExpression_InstanceOf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Instanceof)) {
        return false;
      }
      Instanceof o = (Instanceof) other;
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
    public int compareTo(RelationalExpression other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Instanceof o = (Instanceof) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
