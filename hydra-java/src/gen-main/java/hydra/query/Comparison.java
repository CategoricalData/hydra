package hydra.query;

/**
 * One of several comparison operators
 */
public abstract class Comparison {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/query.Comparison");
  
  private Comparison () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Equal instance) ;
    
    R visit(NotEqual instance) ;
    
    R visit(LessThan instance) ;
    
    R visit(GreaterThan instance) ;
    
    R visit(LessThanOrEqual instance) ;
    
    R visit(GreaterThanOrEqual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Comparison instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Equal instance) {
      return otherwise((instance));
    }
    
    default R visit(NotEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(LessThan instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThan instance) {
      return otherwise((instance));
    }
    
    default R visit(LessThanOrEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThanOrEqual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Equal extends hydra.query.Comparison {
    public Equal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equal)) {
        return false;
      }
      Equal o = (Equal) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NotEqual extends hydra.query.Comparison {
    public NotEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEqual)) {
        return false;
      }
      NotEqual o = (NotEqual) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LessThan extends hydra.query.Comparison {
    public LessThan () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThan)) {
        return false;
      }
      LessThan o = (LessThan) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GreaterThan extends hydra.query.Comparison {
    public GreaterThan () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThan)) {
        return false;
      }
      GreaterThan o = (GreaterThan) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LessThanOrEqual extends hydra.query.Comparison {
    public LessThanOrEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThanOrEqual)) {
        return false;
      }
      LessThanOrEqual o = (LessThanOrEqual) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GreaterThanOrEqual extends hydra.query.Comparison {
    public GreaterThanOrEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThanOrEqual)) {
        return false;
      }
      GreaterThanOrEqual o = (GreaterThanOrEqual) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}