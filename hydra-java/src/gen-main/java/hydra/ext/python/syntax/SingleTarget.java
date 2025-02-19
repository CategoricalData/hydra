// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class SingleTarget implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SingleTarget");
  
  public static final hydra.core.Name FIELD_NAME_SUBSCRIPT_ATTRIBUTE_TARGET = new hydra.core.Name("subscriptAttributeTarget");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PARENS = new hydra.core.Name("parens");
  
  private SingleTarget () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SubscriptAttributeTarget instance) ;
    
    R visit(Name instance) ;
    
    R visit(Parens instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SingleTarget instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SubscriptAttributeTarget instance) {
      return otherwise((instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
  }
  
  public static final class SubscriptAttributeTarget extends hydra.ext.python.syntax.SingleTarget implements Serializable {
    public final hydra.ext.python.syntax.SingleSubscriptAttributeTarget value;
    
    public SubscriptAttributeTarget (hydra.ext.python.syntax.SingleSubscriptAttributeTarget value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubscriptAttributeTarget)) {
        return false;
      }
      SubscriptAttributeTarget o = (SubscriptAttributeTarget) (other);
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
  
  public static final class Name extends hydra.ext.python.syntax.SingleTarget implements Serializable {
    public final hydra.ext.python.syntax.Name value;
    
    public Name (hydra.ext.python.syntax.Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
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
  
  public static final class Parens extends hydra.ext.python.syntax.SingleTarget implements Serializable {
    public final hydra.ext.python.syntax.SingleTarget value;
    
    public Parens (hydra.ext.python.syntax.SingleTarget value) {
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
}