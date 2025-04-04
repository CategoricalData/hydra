// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class RelationalExpression implements Serializable {
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
    
    default R visit(LessThan instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThan instance) {
      return otherwise((instance));
    }
    
    default R visit(LessThanEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThanEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(Instanceof instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Simple extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.ShiftExpression value;
    
    public Simple (hydra.ext.java.syntax.ShiftExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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
  
  public static final class LessThan extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_LessThan value;
    
    public LessThan (hydra.ext.java.syntax.RelationalExpression_LessThan value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThan)) {
        return false;
      }
      LessThan o = (LessThan) (other);
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
  
  public static final class GreaterThan extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_GreaterThan value;
    
    public GreaterThan (hydra.ext.java.syntax.RelationalExpression_GreaterThan value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThan)) {
        return false;
      }
      GreaterThan o = (GreaterThan) (other);
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
  
  public static final class LessThanEqual extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_LessThanEqual value;
    
    public LessThanEqual (hydra.ext.java.syntax.RelationalExpression_LessThanEqual value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThanEqual)) {
        return false;
      }
      LessThanEqual o = (LessThanEqual) (other);
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
  
  public static final class GreaterThanEqual extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual value;
    
    public GreaterThanEqual (hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThanEqual)) {
        return false;
      }
      GreaterThanEqual o = (GreaterThanEqual) (other);
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
  
  public static final class Instanceof extends hydra.ext.java.syntax.RelationalExpression implements Serializable {
    public final hydra.ext.java.syntax.RelationalExpression_InstanceOf value;
    
    public Instanceof (hydra.ext.java.syntax.RelationalExpression_InstanceOf value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Instanceof)) {
        return false;
      }
      Instanceof o = (Instanceof) (other);
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