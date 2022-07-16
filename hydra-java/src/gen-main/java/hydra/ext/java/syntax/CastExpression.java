package hydra.ext.java.syntax;

public abstract class CastExpression {
  private CastExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primitive instance) ;
    
    R visit(NotPlusMinus instance) ;
    
    R visit(Lambda instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CastExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
    
    default R visit(NotPlusMinus instance) {
      return otherwise((instance));
    }
    
    default R visit(Lambda instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primitive extends CastExpression {
    public final CastExpression_Primitive value;
    
    public Primitive (CastExpression_Primitive value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) (other);
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
  
  public static final class NotPlusMinus extends CastExpression {
    public final CastExpression_NotPlusMinus value;
    
    public NotPlusMinus (CastExpression_NotPlusMinus value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotPlusMinus)) {
        return false;
      }
      NotPlusMinus o = (NotPlusMinus) (other);
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
  
  public static final class Lambda extends CastExpression {
    public final CastExpression_Lambda value;
    
    public Lambda (CastExpression_Lambda value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lambda)) {
        return false;
      }
      Lambda o = (Lambda) (other);
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