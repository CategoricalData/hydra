// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public abstract class ObjectPropertyAxiom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/owl/syntax.ObjectPropertyAxiom");
  
  public static final hydra.core.Name FIELD_NAME_ASYMMETRIC_OBJECT_PROPERTY = new hydra.core.Name("asymmetricObjectProperty");
  
  public static final hydra.core.Name FIELD_NAME_DISJOINT_OBJECT_PROPERTIES = new hydra.core.Name("disjointObjectProperties");
  
  public static final hydra.core.Name FIELD_NAME_EQUIVALENT_OBJECT_PROPERTIES = new hydra.core.Name("equivalentObjectProperties");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTIONAL_OBJECT_PROPERTY = new hydra.core.Name("functionalObjectProperty");
  
  public static final hydra.core.Name FIELD_NAME_INVERSE_FUNCTIONAL_OBJECT_PROPERTY = new hydra.core.Name("inverseFunctionalObjectProperty");
  
  public static final hydra.core.Name FIELD_NAME_INVERSE_OBJECT_PROPERTIES = new hydra.core.Name("inverseObjectProperties");
  
  public static final hydra.core.Name FIELD_NAME_IRREFLEXIVE_OBJECT_PROPERTY = new hydra.core.Name("irreflexiveObjectProperty");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_PROPERTY_DOMAIN = new hydra.core.Name("objectPropertyDomain");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_PROPERTY_RANGE = new hydra.core.Name("objectPropertyRange");
  
  public static final hydra.core.Name FIELD_NAME_REFLEXIVE_OBJECT_PROPERTY = new hydra.core.Name("reflexiveObjectProperty");
  
  public static final hydra.core.Name FIELD_NAME_SUB_OBJECT_PROPERTY_OF = new hydra.core.Name("subObjectPropertyOf");
  
  public static final hydra.core.Name FIELD_NAME_SYMMETRIC_OBJECT_PROPERTY = new hydra.core.Name("symmetricObjectProperty");
  
  public static final hydra.core.Name FIELD_NAME_TRANSITIVE_OBJECT_PROPERTY = new hydra.core.Name("transitiveObjectProperty");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AsymmetricObjectProperty instance) {
      return otherwise((instance));
    }
    
    default R visit(DisjointObjectProperties instance) {
      return otherwise((instance));
    }
    
    default R visit(EquivalentObjectProperties instance) {
      return otherwise((instance));
    }
    
    default R visit(FunctionalObjectProperty instance) {
      return otherwise((instance));
    }
    
    default R visit(InverseFunctionalObjectProperty instance) {
      return otherwise((instance));
    }
    
    default R visit(InverseObjectProperties instance) {
      return otherwise((instance));
    }
    
    default R visit(IrreflexiveObjectProperty instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectPropertyDomain instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectPropertyRange instance) {
      return otherwise((instance));
    }
    
    default R visit(ReflexiveObjectProperty instance) {
      return otherwise((instance));
    }
    
    default R visit(SubObjectPropertyOf instance) {
      return otherwise((instance));
    }
    
    default R visit(SymmetricObjectProperty instance) {
      return otherwise((instance));
    }
    
    default R visit(TransitiveObjectProperty instance) {
      return otherwise((instance));
    }
  }
  
  public static final class AsymmetricObjectProperty extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.AsymmetricObjectProperty value;
    
    public AsymmetricObjectProperty (hydra.langs.owl.syntax.AsymmetricObjectProperty value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AsymmetricObjectProperty)) {
        return false;
      }
      AsymmetricObjectProperty o = (AsymmetricObjectProperty) (other);
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
  
  public static final class DisjointObjectProperties extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.DisjointObjectProperties value;
    
    public DisjointObjectProperties (hydra.langs.owl.syntax.DisjointObjectProperties value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DisjointObjectProperties)) {
        return false;
      }
      DisjointObjectProperties o = (DisjointObjectProperties) (other);
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
  
  public static final class EquivalentObjectProperties extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.EquivalentObjectProperties value;
    
    public EquivalentObjectProperties (hydra.langs.owl.syntax.EquivalentObjectProperties value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EquivalentObjectProperties)) {
        return false;
      }
      EquivalentObjectProperties o = (EquivalentObjectProperties) (other);
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
  
  public static final class FunctionalObjectProperty extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.FunctionalObjectProperty value;
    
    public FunctionalObjectProperty (hydra.langs.owl.syntax.FunctionalObjectProperty value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionalObjectProperty)) {
        return false;
      }
      FunctionalObjectProperty o = (FunctionalObjectProperty) (other);
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
  
  public static final class InverseFunctionalObjectProperty extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.InverseFunctionalObjectProperty value;
    
    public InverseFunctionalObjectProperty (hydra.langs.owl.syntax.InverseFunctionalObjectProperty value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InverseFunctionalObjectProperty)) {
        return false;
      }
      InverseFunctionalObjectProperty o = (InverseFunctionalObjectProperty) (other);
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
  
  public static final class InverseObjectProperties extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.InverseObjectProperties value;
    
    public InverseObjectProperties (hydra.langs.owl.syntax.InverseObjectProperties value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InverseObjectProperties)) {
        return false;
      }
      InverseObjectProperties o = (InverseObjectProperties) (other);
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
  
  public static final class IrreflexiveObjectProperty extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.IrreflexiveObjectProperty value;
    
    public IrreflexiveObjectProperty (hydra.langs.owl.syntax.IrreflexiveObjectProperty value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IrreflexiveObjectProperty)) {
        return false;
      }
      IrreflexiveObjectProperty o = (IrreflexiveObjectProperty) (other);
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
  
  public static final class ObjectPropertyDomain extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.ObjectPropertyDomain value;
    
    public ObjectPropertyDomain (hydra.langs.owl.syntax.ObjectPropertyDomain value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectPropertyDomain)) {
        return false;
      }
      ObjectPropertyDomain o = (ObjectPropertyDomain) (other);
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
  
  public static final class ObjectPropertyRange extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.ObjectPropertyRange value;
    
    public ObjectPropertyRange (hydra.langs.owl.syntax.ObjectPropertyRange value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectPropertyRange)) {
        return false;
      }
      ObjectPropertyRange o = (ObjectPropertyRange) (other);
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
  
  public static final class ReflexiveObjectProperty extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.ReflexiveObjectProperty value;
    
    public ReflexiveObjectProperty (hydra.langs.owl.syntax.ReflexiveObjectProperty value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReflexiveObjectProperty)) {
        return false;
      }
      ReflexiveObjectProperty o = (ReflexiveObjectProperty) (other);
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
  
  public static final class SubObjectPropertyOf extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.SubObjectPropertyOf value;
    
    public SubObjectPropertyOf (hydra.langs.owl.syntax.SubObjectPropertyOf value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubObjectPropertyOf)) {
        return false;
      }
      SubObjectPropertyOf o = (SubObjectPropertyOf) (other);
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
  
  public static final class SymmetricObjectProperty extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.SymmetricObjectProperty value;
    
    public SymmetricObjectProperty (hydra.langs.owl.syntax.SymmetricObjectProperty value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SymmetricObjectProperty)) {
        return false;
      }
      SymmetricObjectProperty o = (SymmetricObjectProperty) (other);
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
  
  public static final class TransitiveObjectProperty extends hydra.langs.owl.syntax.ObjectPropertyAxiom implements Serializable {
    public final hydra.langs.owl.syntax.TransitiveObjectProperty value;
    
    public TransitiveObjectProperty (hydra.langs.owl.syntax.TransitiveObjectProperty value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TransitiveObjectProperty)) {
        return false;
      }
      TransitiveObjectProperty o = (TransitiveObjectProperty) (other);
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