// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public abstract class ObjectPropertyAxiom implements Serializable, Comparable<ObjectPropertyAxiom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.ObjectPropertyAxiom");

  public static final hydra.core.Name ASYMMETRIC_OBJECT_PROPERTY = new hydra.core.Name("asymmetricObjectProperty");

  public static final hydra.core.Name DISJOINT_OBJECT_PROPERTIES = new hydra.core.Name("disjointObjectProperties");

  public static final hydra.core.Name EQUIVALENT_OBJECT_PROPERTIES = new hydra.core.Name("equivalentObjectProperties");

  public static final hydra.core.Name FUNCTIONAL_OBJECT_PROPERTY = new hydra.core.Name("functionalObjectProperty");

  public static final hydra.core.Name INVERSE_FUNCTIONAL_OBJECT_PROPERTY = new hydra.core.Name("inverseFunctionalObjectProperty");

  public static final hydra.core.Name INVERSE_OBJECT_PROPERTIES = new hydra.core.Name("inverseObjectProperties");

  public static final hydra.core.Name IRREFLEXIVE_OBJECT_PROPERTY = new hydra.core.Name("irreflexiveObjectProperty");

  public static final hydra.core.Name OBJECT_PROPERTY_DOMAIN = new hydra.core.Name("objectPropertyDomain");

  public static final hydra.core.Name OBJECT_PROPERTY_RANGE = new hydra.core.Name("objectPropertyRange");

  public static final hydra.core.Name REFLEXIVE_OBJECT_PROPERTY = new hydra.core.Name("reflexiveObjectProperty");

  public static final hydra.core.Name SUB_OBJECT_PROPERTY_OF = new hydra.core.Name("subObjectPropertyOf");

  public static final hydra.core.Name SYMMETRIC_OBJECT_PROPERTY = new hydra.core.Name("symmetricObjectProperty");

  public static final hydra.core.Name TRANSITIVE_OBJECT_PROPERTY = new hydra.core.Name("transitiveObjectProperty");

  private ObjectPropertyAxiom () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(AsymmetricObjectProperty instance) ;

    R visit(DisjointObjectProperties instance) ;

    R visit(EquivalentObjectProperties instance) ;

    R visit(FunctionalObjectProperty instance) ;

    R visit(InverseFunctionalObjectProperty instance) ;

    R visit(InverseObjectProperties instance) ;

    R visit(IrreflexiveObjectProperty instance) ;

    R visit(ObjectPropertyDomain instance) ;

    R visit(ObjectPropertyRange instance) ;

    R visit(ReflexiveObjectProperty instance) ;

    R visit(SubObjectPropertyOf instance) ;

    R visit(SymmetricObjectProperty instance) ;

    R visit(TransitiveObjectProperty instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ObjectPropertyAxiom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(AsymmetricObjectProperty instance) {
      return otherwise(instance);
    }

    default R visit(DisjointObjectProperties instance) {
      return otherwise(instance);
    }

    default R visit(EquivalentObjectProperties instance) {
      return otherwise(instance);
    }

    default R visit(FunctionalObjectProperty instance) {
      return otherwise(instance);
    }

    default R visit(InverseFunctionalObjectProperty instance) {
      return otherwise(instance);
    }

    default R visit(InverseObjectProperties instance) {
      return otherwise(instance);
    }

    default R visit(IrreflexiveObjectProperty instance) {
      return otherwise(instance);
    }

    default R visit(ObjectPropertyDomain instance) {
      return otherwise(instance);
    }

    default R visit(ObjectPropertyRange instance) {
      return otherwise(instance);
    }

    default R visit(ReflexiveObjectProperty instance) {
      return otherwise(instance);
    }

    default R visit(SubObjectPropertyOf instance) {
      return otherwise(instance);
    }

    default R visit(SymmetricObjectProperty instance) {
      return otherwise(instance);
    }

    default R visit(TransitiveObjectProperty instance) {
      return otherwise(instance);
    }
  }

  public static final class AsymmetricObjectProperty extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.AsymmetricObjectProperty value;

    public AsymmetricObjectProperty (hydra.owl.syntax.AsymmetricObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AsymmetricObjectProperty)) {
        return false;
      }
      AsymmetricObjectProperty o = (AsymmetricObjectProperty) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AsymmetricObjectProperty o = (AsymmetricObjectProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DisjointObjectProperties extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.DisjointObjectProperties value;

    public DisjointObjectProperties (hydra.owl.syntax.DisjointObjectProperties value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DisjointObjectProperties)) {
        return false;
      }
      DisjointObjectProperties o = (DisjointObjectProperties) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DisjointObjectProperties o = (DisjointObjectProperties) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class EquivalentObjectProperties extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.EquivalentObjectProperties value;

    public EquivalentObjectProperties (hydra.owl.syntax.EquivalentObjectProperties value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EquivalentObjectProperties)) {
        return false;
      }
      EquivalentObjectProperties o = (EquivalentObjectProperties) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EquivalentObjectProperties o = (EquivalentObjectProperties) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class FunctionalObjectProperty extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.FunctionalObjectProperty value;

    public FunctionalObjectProperty (hydra.owl.syntax.FunctionalObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionalObjectProperty)) {
        return false;
      }
      FunctionalObjectProperty o = (FunctionalObjectProperty) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FunctionalObjectProperty o = (FunctionalObjectProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class InverseFunctionalObjectProperty extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.InverseFunctionalObjectProperty value;

    public InverseFunctionalObjectProperty (hydra.owl.syntax.InverseFunctionalObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InverseFunctionalObjectProperty)) {
        return false;
      }
      InverseFunctionalObjectProperty o = (InverseFunctionalObjectProperty) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InverseFunctionalObjectProperty o = (InverseFunctionalObjectProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class InverseObjectProperties extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.InverseObjectProperties value;

    public InverseObjectProperties (hydra.owl.syntax.InverseObjectProperties value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InverseObjectProperties)) {
        return false;
      }
      InverseObjectProperties o = (InverseObjectProperties) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InverseObjectProperties o = (InverseObjectProperties) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class IrreflexiveObjectProperty extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.IrreflexiveObjectProperty value;

    public IrreflexiveObjectProperty (hydra.owl.syntax.IrreflexiveObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IrreflexiveObjectProperty)) {
        return false;
      }
      IrreflexiveObjectProperty o = (IrreflexiveObjectProperty) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IrreflexiveObjectProperty o = (IrreflexiveObjectProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ObjectPropertyDomain extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.ObjectPropertyDomain value;

    public ObjectPropertyDomain (hydra.owl.syntax.ObjectPropertyDomain value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectPropertyDomain)) {
        return false;
      }
      ObjectPropertyDomain o = (ObjectPropertyDomain) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectPropertyDomain o = (ObjectPropertyDomain) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ObjectPropertyRange extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.ObjectPropertyRange value;

    public ObjectPropertyRange (hydra.owl.syntax.ObjectPropertyRange value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectPropertyRange)) {
        return false;
      }
      ObjectPropertyRange o = (ObjectPropertyRange) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectPropertyRange o = (ObjectPropertyRange) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ReflexiveObjectProperty extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.ReflexiveObjectProperty value;

    public ReflexiveObjectProperty (hydra.owl.syntax.ReflexiveObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReflexiveObjectProperty)) {
        return false;
      }
      ReflexiveObjectProperty o = (ReflexiveObjectProperty) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ReflexiveObjectProperty o = (ReflexiveObjectProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SubObjectPropertyOf extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.SubObjectPropertyOf value;

    public SubObjectPropertyOf (hydra.owl.syntax.SubObjectPropertyOf value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubObjectPropertyOf)) {
        return false;
      }
      SubObjectPropertyOf o = (SubObjectPropertyOf) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SubObjectPropertyOf o = (SubObjectPropertyOf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SymmetricObjectProperty extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.SymmetricObjectProperty value;

    public SymmetricObjectProperty (hydra.owl.syntax.SymmetricObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SymmetricObjectProperty)) {
        return false;
      }
      SymmetricObjectProperty o = (SymmetricObjectProperty) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SymmetricObjectProperty o = (SymmetricObjectProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TransitiveObjectProperty extends hydra.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.owl.syntax.TransitiveObjectProperty value;

    public TransitiveObjectProperty (hydra.owl.syntax.TransitiveObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TransitiveObjectProperty)) {
        return false;
      }
      TransitiveObjectProperty o = (TransitiveObjectProperty) other;
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
    public int compareTo(ObjectPropertyAxiom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TransitiveObjectProperty o = (TransitiveObjectProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
