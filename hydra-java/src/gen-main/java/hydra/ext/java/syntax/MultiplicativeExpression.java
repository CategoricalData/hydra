package hydra.ext.java.syntax;

public abstract class MultiplicativeExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.MultiplicativeExpression");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Unary instance) {
      return otherwise((instance));
    }
    
    default R visit(Times instance) {
      return otherwise((instance));
    }
    
    default R visit(Divide instance) {
      return otherwise((instance));
    }
    
    default R visit(Mod instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Unary extends hydra.ext.java.syntax.MultiplicativeExpression {
    public final hydra.ext.java.syntax.UnaryExpression value;
    
    public Unary (hydra.ext.java.syntax.UnaryExpression value) {
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
  
  public static final class Times extends hydra.ext.java.syntax.MultiplicativeExpression {
    public final hydra.ext.java.syntax.MultiplicativeExpression_Binary value;
    
    public Times (hydra.ext.java.syntax.MultiplicativeExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Times)) {
        return false;
      }
      Times o = (Times) (other);
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
  
  public static final class Divide extends hydra.ext.java.syntax.MultiplicativeExpression {
    public final hydra.ext.java.syntax.MultiplicativeExpression_Binary value;
    
    public Divide (hydra.ext.java.syntax.MultiplicativeExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Divide)) {
        return false;
      }
      Divide o = (Divide) (other);
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
  
  public static final class Mod extends hydra.ext.java.syntax.MultiplicativeExpression {
    public final hydra.ext.java.syntax.MultiplicativeExpression_Binary value;
    
    public Mod (hydra.ext.java.syntax.MultiplicativeExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mod)) {
        return false;
      }
      Mod o = (Mod) (other);
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