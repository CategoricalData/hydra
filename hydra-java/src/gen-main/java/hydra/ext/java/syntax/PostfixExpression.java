// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class PostfixExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.PostfixExpression");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_POST_INCREMENT = new hydra.core.Name("postIncrement");
  
  public static final hydra.core.Name FIELD_NAME_POST_DECREMENT = new hydra.core.Name("postDecrement");
  
  private PostfixExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primary instance) ;
    
    R visit(Name instance) ;
    
    R visit(PostIncrement instance) ;
    
    R visit(PostDecrement instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PostfixExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primary instance) {
      return otherwise((instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(PostIncrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PostDecrement instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primary extends hydra.ext.java.syntax.PostfixExpression implements Serializable {
    public final hydra.ext.java.syntax.Primary value;
    
    public Primary (hydra.ext.java.syntax.Primary value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) (other);
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
  
  public static final class Name extends hydra.ext.java.syntax.PostfixExpression implements Serializable {
    public final hydra.ext.java.syntax.ExpressionName value;
    
    public Name (hydra.ext.java.syntax.ExpressionName value) {
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
  
  public static final class PostIncrement extends hydra.ext.java.syntax.PostfixExpression implements Serializable {
    public final hydra.ext.java.syntax.PostIncrementExpression value;
    
    public PostIncrement (hydra.ext.java.syntax.PostIncrementExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PostIncrement)) {
        return false;
      }
      PostIncrement o = (PostIncrement) (other);
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
  
  public static final class PostDecrement extends hydra.ext.java.syntax.PostfixExpression implements Serializable {
    public final hydra.ext.java.syntax.PostDecrementExpression value;
    
    public PostDecrement (hydra.ext.java.syntax.PostDecrementExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PostDecrement)) {
        return false;
      }
      PostDecrement o = (PostDecrement) (other);
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