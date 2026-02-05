// Note: this is an automatically generated file. Do not edit.

package hydra.parsing;

import java.io.Serializable;

/**
 * The result of a parse operation
 */
public abstract class ParseResult<A> implements Serializable, Comparable<ParseResult<A>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.parsing.ParseResult");
  
  public static final hydra.core.Name FIELD_NAME_SUCCESS = new hydra.core.Name("success");
  
  public static final hydra.core.Name FIELD_NAME_FAILURE = new hydra.core.Name("failure");
  
  private ParseResult () {
  
  }
  
  public abstract <R> R accept(Visitor<A, R> visitor) ;
  
  public interface Visitor<A, R> {
    R visit(Success<A> instance) ;
    
    R visit(Failure<A> instance) ;
  }
  
  public interface PartialVisitor<A, R> extends Visitor<A, R> {
    default R otherwise(ParseResult<A> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Success<A> instance) {
      return otherwise((instance));
    }
    
    default R visit(Failure<A> instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A successful parse, with a value and the remaining unparsed input
   */
  public static final class Success<A> extends hydra.parsing.ParseResult<A> implements Serializable {
    public final hydra.parsing.ParseSuccess<A> value;
    
    public Success (hydra.parsing.ParseSuccess<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Success)) {
        return false;
      }
      Success o = (Success) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ParseResult other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Success o = (Success) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<A, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A failed parse, with an error message and the remaining input
   */
  public static final class Failure<A> extends hydra.parsing.ParseResult<A> implements Serializable {
    public final hydra.parsing.ParseError value;
    
    public Failure (hydra.parsing.ParseError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Failure)) {
        return false;
      }
      Failure o = (Failure) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ParseResult other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Failure o = (Failure) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<A, R> visitor) {
      return visitor.visit(this);
    }
  }
}
