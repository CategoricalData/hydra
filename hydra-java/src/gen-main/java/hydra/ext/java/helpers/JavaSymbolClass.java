// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.helpers;

import java.io.Serializable;

/**
 * Classification of a Java symbol for code generation
 */
public abstract class JavaSymbolClass implements Serializable, Comparable<JavaSymbolClass> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.helpers.JavaSymbolClass");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT = new hydra.core.Name("constant");
  
  public static final hydra.core.Name FIELD_NAME_NULLARY_FUNCTION = new hydra.core.Name("nullaryFunction");
  
  public static final hydra.core.Name FIELD_NAME_HOISTED_LAMBDA = new hydra.core.Name("hoistedLambda");
  
  public static final hydra.core.Name FIELD_NAME_UNARY_FUNCTION = new hydra.core.Name("unaryFunction");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL_VARIABLE = new hydra.core.Name("localVariable");
  
  private JavaSymbolClass () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Constant instance) ;
    
    R visit(NullaryFunction instance) ;
    
    R visit(HoistedLambda instance) ;
    
    R visit(UnaryFunction instance) ;
    
    R visit(LocalVariable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(JavaSymbolClass instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Constant instance) {
      return otherwise(instance);
    }
    
    default R visit(NullaryFunction instance) {
      return otherwise(instance);
    }
    
    default R visit(HoistedLambda instance) {
      return otherwise(instance);
    }
    
    default R visit(UnaryFunction instance) {
      return otherwise(instance);
    }
    
    default R visit(LocalVariable instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * A constant value
   */
  public static final class Constant extends hydra.ext.java.helpers.JavaSymbolClass implements Serializable {
    public Constant () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(JavaSymbolClass other) {
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
  
  /**
   * A nullary function (no arguments)
   */
  public static final class NullaryFunction extends hydra.ext.java.helpers.JavaSymbolClass implements Serializable {
    public NullaryFunction () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullaryFunction)) {
        return false;
      }
      NullaryFunction o = (NullaryFunction) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(JavaSymbolClass other) {
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
  
  /**
   * A hoisted lambda wrapped in type lambdas. The Int is the number of curried lambda parameters.
   */
  public static final class HoistedLambda extends hydra.ext.java.helpers.JavaSymbolClass implements Serializable {
    public final Integer value;
    
    public HoistedLambda (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HoistedLambda)) {
        return false;
      }
      HoistedLambda o = (HoistedLambda) other;
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
    public int compareTo(JavaSymbolClass other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HoistedLambda o = (HoistedLambda) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A unary function (single argument)
   */
  public static final class UnaryFunction extends hydra.ext.java.helpers.JavaSymbolClass implements Serializable {
    public UnaryFunction () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnaryFunction)) {
        return false;
      }
      UnaryFunction o = (UnaryFunction) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(JavaSymbolClass other) {
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
  
  /**
   * A local variable
   */
  public static final class LocalVariable extends hydra.ext.java.helpers.JavaSymbolClass implements Serializable {
    public LocalVariable () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalVariable)) {
        return false;
      }
      LocalVariable o = (LocalVariable) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(JavaSymbolClass other) {
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
