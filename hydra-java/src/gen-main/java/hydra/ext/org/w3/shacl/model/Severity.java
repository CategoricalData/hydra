// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

public abstract class Severity implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.shacl.model.Severity");
  
  public static final hydra.core.Name FIELD_NAME_INFO = new hydra.core.Name("info");
  
  public static final hydra.core.Name FIELD_NAME_WARNING = new hydra.core.Name("warning");
  
  public static final hydra.core.Name FIELD_NAME_VIOLATION = new hydra.core.Name("violation");
  
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
  public static final class Info extends hydra.ext.org.w3.shacl.model.Severity implements Serializable {
    public Info () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Info)) {
        return false;
      }
      Info o = (Info) (other);
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
  
  /**
   * A non-critical constraint violation indicating a warning
   */
  public static final class Warning extends hydra.ext.org.w3.shacl.model.Severity implements Serializable {
    public Warning () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Warning)) {
        return false;
      }
      Warning o = (Warning) (other);
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
  
  /**
   * A constraint violation
   */
  public static final class Violation extends hydra.ext.org.w3.shacl.model.Severity implements Serializable {
    public Violation () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Violation)) {
        return false;
      }
      Violation o = (Violation) (other);
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