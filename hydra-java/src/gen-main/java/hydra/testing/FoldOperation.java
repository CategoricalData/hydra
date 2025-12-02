// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A predefined fold operation for testing foldOverTerm
 */
public abstract class FoldOperation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.FoldOperation");
  
  public static final hydra.core.Name FIELD_NAME_SUM_INT32_LITERALS = new hydra.core.Name("sumInt32Literals");
  
  public static final hydra.core.Name FIELD_NAME_COLLECT_LIST_LENGTHS = new hydra.core.Name("collectListLengths");
  
  public static final hydra.core.Name FIELD_NAME_COLLECT_LABELS = new hydra.core.Name("collectLabels");
  
  private FoldOperation () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SumInt32Literals instance) ;
    
    R visit(CollectListLengths instance) ;
    
    R visit(CollectLabels instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FoldOperation instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SumInt32Literals instance) {
      return otherwise((instance));
    }
    
    default R visit(CollectListLengths instance) {
      return otherwise((instance));
    }
    
    default R visit(CollectLabels instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Sum all Int32 literals in a term
   */
  public static final class SumInt32Literals extends hydra.testing.FoldOperation implements Serializable {
    public final Boolean value;
    
    public SumInt32Literals (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SumInt32Literals)) {
        return false;
      }
      SumInt32Literals o = (SumInt32Literals) (other);
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
   * Collect the lengths of all list terms (returns list of integers in traversal order)
   */
  public static final class CollectListLengths extends hydra.testing.FoldOperation implements Serializable {
    public final Boolean value;
    
    public CollectListLengths (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CollectListLengths)) {
        return false;
      }
      CollectListLengths o = (CollectListLengths) (other);
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
   * Collect labels (first element of pairs where first is a string literal)
   */
  public static final class CollectLabels extends hydra.testing.FoldOperation implements Serializable {
    public final Boolean value;
    
    public CollectLabels (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CollectLabels)) {
        return false;
      }
      CollectLabels o = (CollectLabels) (other);
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
