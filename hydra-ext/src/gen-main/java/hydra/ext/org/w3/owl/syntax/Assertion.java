// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class Assertion implements Serializable, Comparable<Assertion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.Assertion");
  
  public static final hydra.core.Name CLASS_ASSERTION = new hydra.core.Name("classAssertion");
  
  public static final hydra.core.Name DATA_PROPERTY_ASSERTION = new hydra.core.Name("dataPropertyAssertion");
  
  public static final hydra.core.Name DIFFERENT_INDIVIDUALS = new hydra.core.Name("differentIndividuals");
  
  public static final hydra.core.Name OBJECT_PROPERTY_ASSERTION = new hydra.core.Name("objectPropertyAssertion");
  
  public static final hydra.core.Name NEGATIVE_DATA_PROPERTY_ASSERTION = new hydra.core.Name("negativeDataPropertyAssertion");
  
  public static final hydra.core.Name NEGATIVE_OBJECT_PROPERTY_ASSERTION = new hydra.core.Name("negativeObjectPropertyAssertion");
  
  public static final hydra.core.Name SAME_INDIVIDUAL = new hydra.core.Name("sameIndividual");
  
  private Assertion () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ClassAssertion instance) ;
    
    R visit(DataPropertyAssertion instance) ;
    
    R visit(DifferentIndividuals instance) ;
    
    R visit(ObjectPropertyAssertion instance) ;
    
    R visit(NegativeDataPropertyAssertion instance) ;
    
    R visit(NegativeObjectPropertyAssertion instance) ;
    
    R visit(SameIndividual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Assertion instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(ClassAssertion instance) {
      return otherwise(instance);
    }
    
    default R visit(DataPropertyAssertion instance) {
      return otherwise(instance);
    }
    
    default R visit(DifferentIndividuals instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectPropertyAssertion instance) {
      return otherwise(instance);
    }
    
    default R visit(NegativeDataPropertyAssertion instance) {
      return otherwise(instance);
    }
    
    default R visit(NegativeObjectPropertyAssertion instance) {
      return otherwise(instance);
    }
    
    default R visit(SameIndividual instance) {
      return otherwise(instance);
    }
  }
  
  public static final class ClassAssertion extends hydra.ext.org.w3.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ClassAssertion value;
    
    public ClassAssertion (hydra.ext.org.w3.owl.syntax.ClassAssertion value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassAssertion)) {
        return false;
      }
      ClassAssertion o = (ClassAssertion) other;
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
    public int compareTo(Assertion other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClassAssertion o = (ClassAssertion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DataPropertyAssertion extends hydra.ext.org.w3.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataPropertyAssertion value;
    
    public DataPropertyAssertion (hydra.ext.org.w3.owl.syntax.DataPropertyAssertion value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataPropertyAssertion)) {
        return false;
      }
      DataPropertyAssertion o = (DataPropertyAssertion) other;
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
    public int compareTo(Assertion other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataPropertyAssertion o = (DataPropertyAssertion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DifferentIndividuals extends hydra.ext.org.w3.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DifferentIndividuals value;
    
    public DifferentIndividuals (hydra.ext.org.w3.owl.syntax.DifferentIndividuals value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DifferentIndividuals)) {
        return false;
      }
      DifferentIndividuals o = (DifferentIndividuals) other;
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
    public int compareTo(Assertion other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DifferentIndividuals o = (DifferentIndividuals) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectPropertyAssertion extends hydra.ext.org.w3.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion value;
    
    public ObjectPropertyAssertion (hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectPropertyAssertion)) {
        return false;
      }
      ObjectPropertyAssertion o = (ObjectPropertyAssertion) other;
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
    public int compareTo(Assertion other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectPropertyAssertion o = (ObjectPropertyAssertion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NegativeDataPropertyAssertion extends hydra.ext.org.w3.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion value;
    
    public NegativeDataPropertyAssertion (hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NegativeDataPropertyAssertion)) {
        return false;
      }
      NegativeDataPropertyAssertion o = (NegativeDataPropertyAssertion) other;
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
    public int compareTo(Assertion other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NegativeDataPropertyAssertion o = (NegativeDataPropertyAssertion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NegativeObjectPropertyAssertion extends hydra.ext.org.w3.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion value;
    
    public NegativeObjectPropertyAssertion (hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NegativeObjectPropertyAssertion)) {
        return false;
      }
      NegativeObjectPropertyAssertion o = (NegativeObjectPropertyAssertion) other;
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
    public int compareTo(Assertion other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NegativeObjectPropertyAssertion o = (NegativeObjectPropertyAssertion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class SameIndividual extends hydra.ext.org.w3.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.SameIndividual value;
    
    public SameIndividual (hydra.ext.org.w3.owl.syntax.SameIndividual value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SameIndividual)) {
        return false;
      }
      SameIndividual o = (SameIndividual) other;
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
    public int compareTo(Assertion other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SameIndividual o = (SameIndividual) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
