package hydra.evaluation;

/**
 * Indicates either the 'out' or the 'in' direction of a step
 */
public abstract class StepDirection {
  private StepDirection () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Out instance) ;
    
    R visit(In instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StepDirection instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Out instance) {
      return otherwise((instance));
    }
    
    default R visit(In instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Out extends StepDirection {
    public Out () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Out)) {
        return false;
      }
      Out o = (Out) (other);
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
  
  public static final class In extends StepDirection {
    public In () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) (other);
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