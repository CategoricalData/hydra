package hydra.ext.java.syntax;

public abstract class UnaryExpressionNotPlusMinus {
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Postfix instance) {
      return otherwise((instance));
    }
    
    default R visit(Tilde instance) {
      return otherwise((instance));
    }
    
    default R visit(Not instance) {
      return otherwise((instance));
    }
    
    default R visit(Cast instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Postfix extends hydra.ext.java.syntax.UnaryExpressionNotPlusMinus {
    public final hydra.ext.java.syntax.PostfixExpression value;
    
    public Postfix (hydra.ext.java.syntax.PostfixExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Postfix)) {
        return false;
      }
      Postfix o = (Postfix) (other);
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
  
  public static final class Tilde extends hydra.ext.java.syntax.UnaryExpressionNotPlusMinus {
    public final hydra.ext.java.syntax.UnaryExpression value;
    
    public Tilde (hydra.ext.java.syntax.UnaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tilde)) {
        return false;
      }
      Tilde o = (Tilde) (other);
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
  
  public static final class Not extends hydra.ext.java.syntax.UnaryExpressionNotPlusMinus {
    public final hydra.ext.java.syntax.UnaryExpression value;
    
    public Not (hydra.ext.java.syntax.UnaryExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) (other);
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
  
  public static final class Cast extends hydra.ext.java.syntax.UnaryExpressionNotPlusMinus {
    public final hydra.ext.java.syntax.CastExpression value;
    
    public Cast (hydra.ext.java.syntax.CastExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cast)) {
        return false;
      }
      Cast o = (Cast) (other);
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