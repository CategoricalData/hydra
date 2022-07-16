package hydra.ext.java.syntax;

public abstract class ShiftExpression {
  private ShiftExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Unary instance) ;
    
    R visit(ShiftLeft instance) ;
    
    R visit(ShiftRight instance) ;
    
    R visit(ShiftRightZeroFill instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShiftExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Unary instance) {
      return otherwise((instance));
    }
    
    default R visit(ShiftLeft instance) {
      return otherwise((instance));
    }
    
    default R visit(ShiftRight instance) {
      return otherwise((instance));
    }
    
    default R visit(ShiftRightZeroFill instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Unary extends ShiftExpression {
    public final AdditiveExpression value;
    
    public Unary (AdditiveExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unary)) {
        return false;
      }
      Unary o = (Unary) (other);
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
  
  public static final class ShiftLeft extends ShiftExpression {
    public final ShiftExpression_Binary value;
    
    public ShiftLeft (ShiftExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShiftLeft)) {
        return false;
      }
      ShiftLeft o = (ShiftLeft) (other);
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
  
  public static final class ShiftRight extends ShiftExpression {
    public final ShiftExpression_Binary value;
    
    public ShiftRight (ShiftExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShiftRight)) {
        return false;
      }
      ShiftRight o = (ShiftRight) (other);
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
  
  public static final class ShiftRightZeroFill extends ShiftExpression {
    public final ShiftExpression_Binary value;
    
    public ShiftRightZeroFill (ShiftExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShiftRightZeroFill)) {
        return false;
      }
      ShiftRightZeroFill o = (ShiftRightZeroFill) (other);
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