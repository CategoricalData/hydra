// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public abstract class Assertion implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.Assertion");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_ASSERTION = new hydra.core.Name("classAssertion");
  
  public static final hydra.core.Name FIELD_NAME_DATA_PROPERTY_ASSERTION = new hydra.core.Name("dataPropertyAssertion");
  
  public static final hydra.core.Name FIELD_NAME_DIFFERENT_INDIVIDUALS = new hydra.core.Name("differentIndividuals");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_PROPERTY_ASSERTION = new hydra.core.Name("objectPropertyAssertion");
  
  public static final hydra.core.Name FIELD_NAME_NEGATIVE_DATA_PROPERTY_ASSERTION = new hydra.core.Name("negativeDataPropertyAssertion");
  
  public static final hydra.core.Name FIELD_NAME_NEGATIVE_OBJECT_PROPERTY_ASSERTION = new hydra.core.Name("negativeObjectPropertyAssertion");
  
  public static final hydra.core.Name FIELD_NAME_SAME_INDIVIDUAL = new hydra.core.Name("sameIndividual");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ClassAssertion instance) {
      return otherwise((instance));
    }
    
    default R visit(DataPropertyAssertion instance) {
      return otherwise((instance));
    }
    
    default R visit(DifferentIndividuals instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectPropertyAssertion instance) {
      return otherwise((instance));
    }
    
    default R visit(NegativeDataPropertyAssertion instance) {
      return otherwise((instance));
    }
    
    default R visit(NegativeObjectPropertyAssertion instance) {
      return otherwise((instance));
    }
    
    default R visit(SameIndividual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ClassAssertion extends hydra.ext.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.owl.syntax.ClassAssertion value;
    
    public ClassAssertion (hydra.ext.owl.syntax.ClassAssertion value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassAssertion)) {
        return false;
      }
      ClassAssertion o = (ClassAssertion) (other);
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
  
  public static final class DataPropertyAssertion extends hydra.ext.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.owl.syntax.DataPropertyAssertion value;
    
    public DataPropertyAssertion (hydra.ext.owl.syntax.DataPropertyAssertion value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataPropertyAssertion)) {
        return false;
      }
      DataPropertyAssertion o = (DataPropertyAssertion) (other);
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
  
  public static final class DifferentIndividuals extends hydra.ext.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.owl.syntax.DifferentIndividuals value;
    
    public DifferentIndividuals (hydra.ext.owl.syntax.DifferentIndividuals value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DifferentIndividuals)) {
        return false;
      }
      DifferentIndividuals o = (DifferentIndividuals) (other);
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
  
  public static final class ObjectPropertyAssertion extends hydra.ext.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.owl.syntax.ObjectPropertyAssertion value;
    
    public ObjectPropertyAssertion (hydra.ext.owl.syntax.ObjectPropertyAssertion value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectPropertyAssertion)) {
        return false;
      }
      ObjectPropertyAssertion o = (ObjectPropertyAssertion) (other);
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
  
  public static final class NegativeDataPropertyAssertion extends hydra.ext.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.owl.syntax.NegativeDataPropertyAssertion value;
    
    public NegativeDataPropertyAssertion (hydra.ext.owl.syntax.NegativeDataPropertyAssertion value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NegativeDataPropertyAssertion)) {
        return false;
      }
      NegativeDataPropertyAssertion o = (NegativeDataPropertyAssertion) (other);
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
  
  public static final class NegativeObjectPropertyAssertion extends hydra.ext.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.owl.syntax.NegativeObjectPropertyAssertion value;
    
    public NegativeObjectPropertyAssertion (hydra.ext.owl.syntax.NegativeObjectPropertyAssertion value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NegativeObjectPropertyAssertion)) {
        return false;
      }
      NegativeObjectPropertyAssertion o = (NegativeObjectPropertyAssertion) (other);
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
  
  public static final class SameIndividual extends hydra.ext.owl.syntax.Assertion implements Serializable {
    public final hydra.ext.owl.syntax.SameIndividual value;
    
    public SameIndividual (hydra.ext.owl.syntax.SameIndividual value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SameIndividual)) {
        return false;
      }
      SameIndividual o = (SameIndividual) (other);
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
