// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class Annotation implements Serializable, Comparable<Annotation> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.Annotation");
  
  public static final hydra.core.Name FIELD_NAME_NORMAL = new hydra.core.Name("normal");
  
  public static final hydra.core.Name FIELD_NAME_MARKER = new hydra.core.Name("marker");
  
  public static final hydra.core.Name FIELD_NAME_SINGLE_ELEMENT = new hydra.core.Name("singleElement");
  
  private Annotation () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Normal instance) ;
    
    R visit(Marker instance) ;
    
    R visit(SingleElement instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Annotation instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Normal instance) {
      return otherwise(instance);
    }
    
    default R visit(Marker instance) {
      return otherwise(instance);
    }
    
    default R visit(SingleElement instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Normal extends hydra.ext.java.syntax.Annotation implements Serializable {
    public final hydra.ext.java.syntax.NormalAnnotation value;
    
    public Normal (hydra.ext.java.syntax.NormalAnnotation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Normal)) {
        return false;
      }
      Normal o = (Normal) other;
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
    public int compareTo(Annotation other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Normal o = (Normal) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Marker extends hydra.ext.java.syntax.Annotation implements Serializable {
    public final hydra.ext.java.syntax.MarkerAnnotation value;
    
    public Marker (hydra.ext.java.syntax.MarkerAnnotation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Marker)) {
        return false;
      }
      Marker o = (Marker) other;
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
    public int compareTo(Annotation other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Marker o = (Marker) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class SingleElement extends hydra.ext.java.syntax.Annotation implements Serializable {
    public final hydra.ext.java.syntax.SingleElementAnnotation value;
    
    public SingleElement (hydra.ext.java.syntax.SingleElementAnnotation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleElement)) {
        return false;
      }
      SingleElement o = (SingleElement) other;
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
    public int compareTo(Annotation other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SingleElement o = (SingleElement) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
