// Note: this is an automatically generated file. Do not edit.

package hydra.variants;

import java.io.Serializable;

/**
 * The identifier of an elimination constructor
 */
public abstract class EliminationVariant implements Serializable, Comparable<EliminationVariant> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.variants.EliminationVariant");
  
  public static final hydra.core.Name FIELD_NAME_RECORD = new hydra.core.Name("record");
  
  public static final hydra.core.Name FIELD_NAME_UNION = new hydra.core.Name("union");
  
  public static final hydra.core.Name FIELD_NAME_WRAP = new hydra.core.Name("wrap");
  
  private EliminationVariant () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Record instance) ;
    
    R visit(Union instance) ;
    
    R visit(Wrap instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EliminationVariant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Record instance) {
      return otherwise(instance);
    }
    
    default R visit(Union instance) {
      return otherwise(instance);
    }
    
    default R visit(Wrap instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Record extends hydra.variants.EliminationVariant implements Serializable {
    public Record () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EliminationVariant other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Union extends hydra.variants.EliminationVariant implements Serializable {
    public Union () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EliminationVariant other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Wrap extends hydra.variants.EliminationVariant implements Serializable {
    public Wrap () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wrap)) {
        return false;
      }
      Wrap o = (Wrap) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EliminationVariant other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
