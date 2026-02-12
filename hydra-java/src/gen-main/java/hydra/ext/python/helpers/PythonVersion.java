// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.helpers;

import java.io.Serializable;

/**
 * Target Python version for code generation
 */
public abstract class PythonVersion implements Serializable, Comparable<PythonVersion> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.helpers.PythonVersion");
  
  public static final hydra.core.Name FIELD_NAME_PYTHON310 = new hydra.core.Name("python310");
  
  public static final hydra.core.Name FIELD_NAME_PYTHON312 = new hydra.core.Name("python312");
  
  private PythonVersion () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Python310 instance) ;
    
    R visit(Python312 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PythonVersion instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Python310 instance) {
      return otherwise(instance);
    }
    
    default R visit(Python312 instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Python310 extends hydra.ext.python.helpers.PythonVersion implements Serializable {
    public Python310 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Python310)) {
        return false;
      }
      Python310 o = (Python310) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PythonVersion other) {
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
  
  public static final class Python312 extends hydra.ext.python.helpers.PythonVersion implements Serializable {
    public Python312 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Python312)) {
        return false;
      }
      Python312 o = (Python312) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PythonVersion other) {
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
