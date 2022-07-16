package hydra.ext.java.syntax;

public abstract class UnaryExpression {
  private UnaryExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PreIncrement instance) ;
    
    R visit(PreDecrement instance) ;
    
    R visit(Plus instance) ;
    
    R visit(Minus instance) ;
    
    R visit(Other instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PreIncrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PreDecrement instance) {
      return otherwise((instance));
    }
    
    default R visit(Plus instance) {
      return otherwise((instance));
    }
    
    default R visit(Minus instance) {
      return otherwise((instance));
    }
    
    default R visit(Other instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PreIncrement extends UnaryExpression {
    public final PreIncrementExpression value;
    
    public PreIncrement (PreIncrementExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PreIncrement)) {
        return false;
      }
      PreIncrement o = (PreIncrement) (other);
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
  
  public static final class PreDecrement extends UnaryExpression {
    public final PreDecrementExpression value;
    
    public PreDecrement (PreDecrementExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PreDecrement)) {
        return false;
      }
      PreDecrement o = (PreDecrement) (other);
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
  
  public static final class Plus extends UnaryExpression {
    public final UnaryExpression value;
    
    public Plus (UnaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) (other);
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
  
  public static final class Minus extends UnaryExpression {
    public final UnaryExpression value;
    
    public Minus (UnaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minus)) {
        return false;
      }
      Minus o = (Minus) (other);
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
  
  public static final class Other extends UnaryExpression {
    public final UnaryExpressionNotPlusMinus value;
    
    public Other (UnaryExpressionNotPlusMinus value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Other)) {
        return false;
      }
      Other o = (Other) (other);
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