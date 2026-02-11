// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Any of several indentation styles
 */
public abstract class IndentStyle implements Serializable, Comparable<IndentStyle> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.IndentStyle");
  
  public static final hydra.core.Name FIELD_NAME_ALL_LINES = new hydra.core.Name("allLines");
  
  public static final hydra.core.Name FIELD_NAME_SUBSEQUENT_LINES = new hydra.core.Name("subsequentLines");
  
  private IndentStyle () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AllLines instance) ;
    
    R visit(SubsequentLines instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IndentStyle instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(AllLines instance) {
      return otherwise(instance);
    }
    
    default R visit(SubsequentLines instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * Indent all lines with the given string
   */
  public static final class AllLines extends hydra.ast.IndentStyle implements Serializable {
    public final String value;
    
    public AllLines (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AllLines)) {
        return false;
      }
      AllLines o = (AllLines) other;
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
    public int compareTo(IndentStyle other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AllLines o = (AllLines) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Indent only lines after the first with the given string
   */
  public static final class SubsequentLines extends hydra.ast.IndentStyle implements Serializable {
    public final String value;
    
    public SubsequentLines (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubsequentLines)) {
        return false;
      }
      SubsequentLines o = (SubsequentLines) other;
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
    public int compareTo(IndentStyle other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SubsequentLines o = (SubsequentLines) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
