package hydra.evaluation;

/**
 * A qualified result; success with a value or failure with an error message
 */
public abstract class Result<M> {
  private Result () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Success instance) ;
    
    R visit(Failure instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Result instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Success instance) {
      return otherwise((instance));
    }
    
    default R visit(Failure instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Success<M> extends Result<M> {
    public final M value;
    
    public Success (M value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Success)) {
        return false;
      }
      Success o = (Success) (other);
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
  
  public static final class Failure<M> extends Result<M> {
    public final String value;
    
    public Failure (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Failure)) {
        return false;
      }
      Failure o = (Failure) (other);
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