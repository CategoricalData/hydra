package hydra.ext.shacl.model;

public abstract class Severity {
  private Severity () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Info instance) ;
    
    R visit(Warning instance) ;
    
    R visit(Violation instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Severity instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Info instance) {
      return otherwise((instance));
    }
    
    default R visit(Warning instance) {
      return otherwise((instance));
    }
    
    default R visit(Violation instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A non-critical constraint violation indicating an informative message
   */
  public static final class Info extends Severity {
    /**
     * A non-critical constraint violation indicating an informative message
     */
    public final java.lang.Void value;
    
    public Info (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Info)) {
        return false;
      }
      Info o = (Info) (other);
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
  
  /**
   * A non-critical constraint violation indicating a warning
   */
  public static final class Warning extends Severity {
    /**
     * A non-critical constraint violation indicating a warning
     */
    public final java.lang.Void value;
    
    public Warning (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Warning)) {
        return false;
      }
      Warning o = (Warning) (other);
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
  
  /**
   * A constraint violation
   */
  public static final class Violation extends Severity {
    /**
     * A constraint violation
     */
    public final java.lang.Void value;
    
    public Violation (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Violation)) {
        return false;
      }
      Violation o = (Violation) (other);
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