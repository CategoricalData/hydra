// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Axioms
 */
public abstract class Axiom implements Serializable, Comparable<Axiom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.Axiom");
  
  public static final hydra.core.Name ANNOTATION_AXIOM = new hydra.core.Name("annotationAxiom");
  
  public static final hydra.core.Name ASSERTION = new hydra.core.Name("assertion");
  
  public static final hydra.core.Name CLASS_AXIOM = new hydra.core.Name("classAxiom");
  
  public static final hydra.core.Name DATA_PROPERTY_AXIOM = new hydra.core.Name("dataPropertyAxiom");
  
  public static final hydra.core.Name DATATYPE_DEFINITION = new hydra.core.Name("datatypeDefinition");
  
  public static final hydra.core.Name DECLARATION = new hydra.core.Name("declaration");
  
  public static final hydra.core.Name HAS_KEY = new hydra.core.Name("hasKey");
  
  public static final hydra.core.Name OBJECT_PROPERTY_AXIOM = new hydra.core.Name("objectPropertyAxiom");
  
  private Axiom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AnnotationAxiom instance) ;
    
    R visit(Assertion instance) ;
    
    R visit(ClassAxiom instance) ;
    
    R visit(DataPropertyAxiom instance) ;
    
    R visit(DatatypeDefinition instance) ;
    
    R visit(Declaration instance) ;
    
    R visit(HasKey instance) ;
    
    R visit(ObjectPropertyAxiom instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Axiom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(AnnotationAxiom instance) {
      return otherwise(instance);
    }
    
    default R visit(Assertion instance) {
      return otherwise(instance);
    }
    
    default R visit(ClassAxiom instance) {
      return otherwise(instance);
    }
    
    default R visit(DataPropertyAxiom instance) {
      return otherwise(instance);
    }
    
    default R visit(DatatypeDefinition instance) {
      return otherwise(instance);
    }
    
    default R visit(Declaration instance) {
      return otherwise(instance);
    }
    
    default R visit(HasKey instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectPropertyAxiom instance) {
      return otherwise(instance);
    }
  }
  
  public static final class AnnotationAxiom extends hydra.ext.org.w3.owl.syntax.Axiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.AnnotationAxiom value;
    
    public AnnotationAxiom (hydra.ext.org.w3.owl.syntax.AnnotationAxiom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationAxiom)) {
        return false;
      }
      AnnotationAxiom o = (AnnotationAxiom) other;
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
    public int compareTo(Axiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnnotationAxiom o = (AnnotationAxiom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Assertion extends hydra.ext.org.w3.owl.syntax.Axiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.Assertion value;
    
    public Assertion (hydra.ext.org.w3.owl.syntax.Assertion value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assertion)) {
        return false;
      }
      Assertion o = (Assertion) other;
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
    public int compareTo(Axiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assertion o = (Assertion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ClassAxiom extends hydra.ext.org.w3.owl.syntax.Axiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ClassAxiom value;
    
    public ClassAxiom (hydra.ext.org.w3.owl.syntax.ClassAxiom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassAxiom)) {
        return false;
      }
      ClassAxiom o = (ClassAxiom) other;
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
    public int compareTo(Axiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClassAxiom o = (ClassAxiom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DataPropertyAxiom extends hydra.ext.org.w3.owl.syntax.Axiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataPropertyAxiom value;
    
    public DataPropertyAxiom (hydra.ext.org.w3.owl.syntax.DataPropertyAxiom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataPropertyAxiom)) {
        return false;
      }
      DataPropertyAxiom o = (DataPropertyAxiom) other;
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
    public int compareTo(Axiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataPropertyAxiom o = (DataPropertyAxiom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DatatypeDefinition extends hydra.ext.org.w3.owl.syntax.Axiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DatatypeDefinition value;
    
    public DatatypeDefinition (hydra.ext.org.w3.owl.syntax.DatatypeDefinition value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DatatypeDefinition)) {
        return false;
      }
      DatatypeDefinition o = (DatatypeDefinition) other;
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
    public int compareTo(Axiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DatatypeDefinition o = (DatatypeDefinition) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Declaration extends hydra.ext.org.w3.owl.syntax.Axiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.Declaration value;
    
    public Declaration (hydra.ext.org.w3.owl.syntax.Declaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Declaration)) {
        return false;
      }
      Declaration o = (Declaration) other;
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
    public int compareTo(Axiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Declaration o = (Declaration) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class HasKey extends hydra.ext.org.w3.owl.syntax.Axiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.HasKey value;
    
    public HasKey (hydra.ext.org.w3.owl.syntax.HasKey value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasKey)) {
        return false;
      }
      HasKey o = (HasKey) other;
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
    public int compareTo(Axiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HasKey o = (HasKey) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectPropertyAxiom extends hydra.ext.org.w3.owl.syntax.Axiom implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom value;
    
    public ObjectPropertyAxiom (hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectPropertyAxiom)) {
        return false;
      }
      ObjectPropertyAxiom o = (ObjectPropertyAxiom) other;
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
    public int compareTo(Axiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectPropertyAxiom o = (ObjectPropertyAxiom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
