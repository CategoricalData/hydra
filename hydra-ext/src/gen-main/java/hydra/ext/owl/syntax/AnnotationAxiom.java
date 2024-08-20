// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public abstract class AnnotationAxiom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.AnnotationAxiom");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION_ASSERTION = new hydra.core.Name("annotationAssertion");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION_PROPERTY_DOMAIN = new hydra.core.Name("annotationPropertyDomain");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION_PROPERTY_RANGE = new hydra.core.Name("annotationPropertyRange");
  
  public static final hydra.core.Name FIELD_NAME_SUB_ANNOTATION_PROPERTY_OF = new hydra.core.Name("subAnnotationPropertyOf");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AnnotationAssertion instance) {
      return otherwise((instance));
    }
    
    default R visit(AnnotationPropertyDomain instance) {
      return otherwise((instance));
    }
    
    default R visit(AnnotationPropertyRange instance) {
      return otherwise((instance));
    }
    
    default R visit(SubAnnotationPropertyOf instance) {
      return otherwise((instance));
    }
  }
  
  public static final class AnnotationAssertion extends hydra.ext.owl.syntax.AnnotationAxiom implements Serializable {
    public final hydra.ext.owl.syntax.AnnotationAssertion value;
    
    public AnnotationAssertion (hydra.ext.owl.syntax.AnnotationAssertion value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationAssertion)) {
        return false;
      }
      AnnotationAssertion o = (AnnotationAssertion) (other);
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
  
  public static final class AnnotationPropertyDomain extends hydra.ext.owl.syntax.AnnotationAxiom implements Serializable {
    public final hydra.ext.owl.syntax.AnnotationPropertyDomain value;
    
    public AnnotationPropertyDomain (hydra.ext.owl.syntax.AnnotationPropertyDomain value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationPropertyDomain)) {
        return false;
      }
      AnnotationPropertyDomain o = (AnnotationPropertyDomain) (other);
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
  
  public static final class AnnotationPropertyRange extends hydra.ext.owl.syntax.AnnotationAxiom implements Serializable {
    public final hydra.ext.owl.syntax.AnnotationPropertyRange value;
    
    public AnnotationPropertyRange (hydra.ext.owl.syntax.AnnotationPropertyRange value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationPropertyRange)) {
        return false;
      }
      AnnotationPropertyRange o = (AnnotationPropertyRange) (other);
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
  
  public static final class SubAnnotationPropertyOf extends hydra.ext.owl.syntax.AnnotationAxiom implements Serializable {
    public final hydra.ext.owl.syntax.SubAnnotationPropertyOf value;
    
    public SubAnnotationPropertyOf (hydra.ext.owl.syntax.SubAnnotationPropertyOf value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubAnnotationPropertyOf)) {
        return false;
      }
      SubAnnotationPropertyOf o = (SubAnnotationPropertyOf) (other);
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