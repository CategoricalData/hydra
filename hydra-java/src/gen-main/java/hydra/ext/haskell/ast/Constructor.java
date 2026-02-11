// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A data constructor
 */
public abstract class Constructor implements Serializable, Comparable<Constructor> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.Constructor");
  
  public static final hydra.core.Name FIELD_NAME_ORDINARY = new hydra.core.Name("ordinary");
  
  public static final hydra.core.Name FIELD_NAME_RECORD = new hydra.core.Name("record");
  
  private Constructor () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ordinary instance) ;
    
    R visit(Record instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Constructor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Ordinary instance) {
      return otherwise(instance);
    }
    
    default R visit(Record instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * An ordinary (positional) constructor
   */
  public static final class Ordinary extends hydra.ext.haskell.ast.Constructor implements Serializable {
    public final hydra.ext.haskell.ast.OrdinaryConstructor value;
    
    public Ordinary (hydra.ext.haskell.ast.OrdinaryConstructor value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ordinary)) {
        return false;
      }
      Ordinary o = (Ordinary) other;
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
    public int compareTo(Constructor other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ordinary o = (Ordinary) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A record constructor
   */
  public static final class Record extends hydra.ext.haskell.ast.Constructor implements Serializable {
    public final hydra.ext.haskell.ast.RecordConstructor value;
    
    public Record (hydra.ext.haskell.ast.RecordConstructor value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) other;
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
    public int compareTo(Constructor other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Record o = (Record) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
