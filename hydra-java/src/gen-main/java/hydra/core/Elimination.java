// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A corresponding elimination for an introduction term
 */
public abstract class Elimination implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Elimination");
  
  private Elimination () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(List instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Product instance) ;
    
    R visit(Record instance) ;
    
    R visit(Union instance) ;
    
    R visit(Wrap instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Elimination instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Product instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
    
    default R visit(Wrap instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Eliminates a list using a fold function; this function has the signature b -&gt; [a] -&gt; b
   */
  public static final class List extends hydra.core.Elimination implements Serializable {
    public final hydra.core.Term value;
    
    public List (hydra.core.Term value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
   * Eliminates an optional term by matching over the two possible cases
   */
  public static final class Optional extends hydra.core.Elimination implements Serializable {
    public final hydra.core.OptionalCases value;
    
    public Optional (hydra.core.OptionalCases value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
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
   * Eliminates a tuple by projecting the component at a given 0-indexed offset
   */
  public static final class Product extends hydra.core.Elimination implements Serializable {
    public final hydra.core.TupleProjection value;
    
    public Product (hydra.core.TupleProjection value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Product)) {
        return false;
      }
      Product o = (Product) (other);
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
   * Eliminates a record by projecting a given field
   */
  public static final class Record extends hydra.core.Elimination implements Serializable {
    public final hydra.core.Projection value;
    
    public Record (hydra.core.Projection value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) (other);
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
   * Eliminates a union term by matching over the fields of the union. This is a case statement.
   */
  public static final class Union extends hydra.core.Elimination implements Serializable {
    public final hydra.core.CaseStatement value;
    
    public Union (hydra.core.CaseStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
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
   * Unwrap a wrapped term
   */
  public static final class Wrap extends hydra.core.Elimination implements Serializable {
    public final hydra.core.Name value;
    
    public Wrap (hydra.core.Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wrap)) {
        return false;
      }
      Wrap o = (Wrap) (other);
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