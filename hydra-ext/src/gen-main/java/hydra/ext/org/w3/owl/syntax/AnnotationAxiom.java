// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class AnnotationAxiom implements Serializable, Comparable<AnnotationAxiom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationAxiom");
  
  public static final hydra.core.Name ANNOTATION_ASSERTION = new hydra.core.Name("annotationAssertion");
  
  public static final hydra.core.Name ANNOTATION_PROPERTY_DOMAIN = new hydra.core.Name("annotationPropertyDomain");
  
  public static final hydra.core.Name ANNOTATION_PROPERTY_RANGE = new hydra.core.Name("annotationPropertyRange");
  
  public static final hydra.core.Name SUB_ANNOTATION_PROPERTY_OF = new hydra.core.Name("subAnnotationPropertyOf");
  
  private AnnotationAxiom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AnnotationAssertion instance) ;
    
    R visit(AnnotationPropertyDomain instance) ;
    
    R visit(AnnotationPropertyRange instance) ;
    
    R visit(SubAnnotationPropertyOf instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnnotationAxiom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(AnnotationAssertion instance) {
      return otherwise(instance);
    }
    
    default R visit(AnnotationPropertyDomain instance) {
      return otherwise(instance);
    }
    
    default R visit(AnnotationPropertyRange instance) {
      return otherwise(instance);
    }
    
    default R visit(SubAnnotationPropertyOf instance) {
      return otherwise(instance);
    }
  }
  
  public static final class AnnotationAssertion extends hydra.ext.org.w3.owl.syntax.AnnotationAxiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.AnnotationAssertion value;
    
    public AnnotationAssertion (hydra.ext.org.w3.owl.syntax.AnnotationAssertion value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationAssertion)) {
        return false;
      }
      AnnotationAssertion o = (AnnotationAssertion) other;
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
    public int compareTo(AnnotationAxiom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnnotationAssertion o = (AnnotationAssertion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AnnotationPropertyDomain extends hydra.ext.org.w3.owl.syntax.AnnotationAxiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain value;
    
    public AnnotationPropertyDomain (hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationPropertyDomain)) {
        return false;
      }
      AnnotationPropertyDomain o = (AnnotationPropertyDomain) other;
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
    public int compareTo(AnnotationAxiom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnnotationPropertyDomain o = (AnnotationPropertyDomain) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AnnotationPropertyRange extends hydra.ext.org.w3.owl.syntax.AnnotationAxiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange value;
    
    public AnnotationPropertyRange (hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationPropertyRange)) {
        return false;
      }
      AnnotationPropertyRange o = (AnnotationPropertyRange) other;
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
    public int compareTo(AnnotationAxiom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnnotationPropertyRange o = (AnnotationPropertyRange) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class SubAnnotationPropertyOf extends hydra.ext.org.w3.owl.syntax.AnnotationAxiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf value;
    
    public SubAnnotationPropertyOf (hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubAnnotationPropertyOf)) {
        return false;
      }
      SubAnnotationPropertyOf o = (SubAnnotationPropertyOf) other;
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
    public int compareTo(AnnotationAxiom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SubAnnotationPropertyOf o = (SubAnnotationPropertyOf) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
