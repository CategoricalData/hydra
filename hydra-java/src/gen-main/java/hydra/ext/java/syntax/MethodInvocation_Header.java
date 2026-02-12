// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class MethodInvocation_Header implements Serializable, Comparable<MethodInvocation_Header> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodInvocation_Header");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_COMPLEX = new hydra.core.Name("complex");
  
  private MethodInvocation_Header () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(Complex instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MethodInvocation_Header instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Simple instance) {
      return otherwise(instance);
    }
    
    default R visit(Complex instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Simple extends hydra.ext.java.syntax.MethodInvocation_Header implements Serializable {
    public final hydra.ext.java.syntax.MethodName value;
    
    public Simple (hydra.ext.java.syntax.MethodName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(MethodInvocation_Header other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Complex extends hydra.ext.java.syntax.MethodInvocation_Header implements Serializable {
    public final hydra.ext.java.syntax.MethodInvocation_Complex value;
    
    public Complex (hydra.ext.java.syntax.MethodInvocation_Complex value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Complex)) {
        return false;
      }
      Complex o = (Complex) other;
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
    public int compareTo(MethodInvocation_Header other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Complex o = (Complex) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
