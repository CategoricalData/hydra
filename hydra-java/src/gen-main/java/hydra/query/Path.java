// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A query path
 */
public abstract class Path implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.query.Path");
  
  public static final hydra.core.Name FIELD_NAME_STEP = new hydra.core.Name("step");
  
  public static final hydra.core.Name FIELD_NAME_REGEX = new hydra.core.Name("regex");
  
  public static final hydra.core.Name FIELD_NAME_INVERSE = new hydra.core.Name("inverse");
  
  private Path () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Step instance) ;
    
    R visit(Regex instance) ;
    
    R visit(Inverse instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Path instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Step instance) {
      return otherwise((instance));
    }
    
    default R visit(Regex instance) {
      return otherwise((instance));
    }
    
    default R visit(Inverse instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A path given by a single step
   */
  public static final class Step extends hydra.query.Path implements Serializable {
    public final hydra.query.Step value;
    
    public Step (hydra.query.Step value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Step)) {
        return false;
      }
      Step o = (Step) (other);
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
   * A path given by a regular expression quantifier applied to another path
   */
  public static final class Regex extends hydra.query.Path implements Serializable {
    public final hydra.query.RegexSequence value;
    
    public Regex (hydra.query.RegexSequence value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regex)) {
        return false;
      }
      Regex o = (Regex) (other);
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
   * A path given by the inverse of another path
   */
  public static final class Inverse extends hydra.query.Path implements Serializable {
    public final hydra.query.Path value;
    
    public Inverse (hydra.query.Path value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inverse)) {
        return false;
      }
      Inverse o = (Inverse) (other);
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