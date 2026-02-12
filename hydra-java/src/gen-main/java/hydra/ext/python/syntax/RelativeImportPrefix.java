// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class RelativeImportPrefix implements Serializable, Comparable<RelativeImportPrefix> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.RelativeImportPrefix");
  
  public static final hydra.core.Name FIELD_NAME_DOT = new hydra.core.Name("dot");
  
  public static final hydra.core.Name FIELD_NAME_ELLIPSIS = new hydra.core.Name("ellipsis");
  
  private RelativeImportPrefix () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Dot instance) ;
    
    R visit(Ellipsis instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RelativeImportPrefix instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Dot instance) {
      return otherwise(instance);
    }
    
    default R visit(Ellipsis instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Dot extends hydra.ext.python.syntax.RelativeImportPrefix implements Serializable {
    public Dot () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dot)) {
        return false;
      }
      Dot o = (Dot) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(RelativeImportPrefix other) {
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
  
  public static final class Ellipsis extends hydra.ext.python.syntax.RelativeImportPrefix implements Serializable {
    public Ellipsis () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ellipsis)) {
        return false;
      }
      Ellipsis o = (Ellipsis) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(RelativeImportPrefix other) {
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
