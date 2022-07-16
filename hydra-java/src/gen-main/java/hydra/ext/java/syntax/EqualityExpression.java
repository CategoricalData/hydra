package hydra.ext.java.syntax;

public abstract class EqualityExpression {
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Unary instance) {
      return otherwise((instance));
    }
    
    default R visit(Equal instance) {
      return otherwise((instance));
    }
    
    default R visit(NotEqual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Unary extends EqualityExpression {
    public final RelationalExpression value;
    
    public Unary (RelationalExpression value) {
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
  
  public static final class Equal extends EqualityExpression {
    public final EqualityExpression_Binary value;
    
    public Equal (EqualityExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equal)) {
        return false;
      }
      Equal o = (Equal) (other);
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
  
  public static final class NotEqual extends EqualityExpression {
    public final EqualityExpression_Binary value;
    
    public NotEqual (EqualityExpression_Binary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEqual)) {
        return false;
      }
      NotEqual o = (NotEqual) (other);
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