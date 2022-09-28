package hydra.ext.owl.syntax;

public abstract class ObjectPropertyAxiom {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.ObjectPropertyAxiom");
  
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
  
  public static final class AsymmetricObjectProperty extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.AsymmetricObjectProperty value;
    
    public AsymmetricObjectProperty (hydra.ext.owl.syntax.AsymmetricObjectProperty value) {
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
  
  public static final class DisjointObjectProperties extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.DisjointObjectProperties value;
    
    public DisjointObjectProperties (hydra.ext.owl.syntax.DisjointObjectProperties value) {
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
  
  public static final class EquivalentObjectProperties extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.EquivalentObjectProperties value;
    
    public EquivalentObjectProperties (hydra.ext.owl.syntax.EquivalentObjectProperties value) {
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
  
  public static final class FunctionalObjectProperty extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.FunctionalObjectProperty value;
    
    public FunctionalObjectProperty (hydra.ext.owl.syntax.FunctionalObjectProperty value) {
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
  
  public static final class InverseFunctionalObjectProperty extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.InverseFunctionalObjectProperty value;
    
    public InverseFunctionalObjectProperty (hydra.ext.owl.syntax.InverseFunctionalObjectProperty value) {
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
  
  public static final class InverseObjectProperties extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.InverseObjectProperties value;
    
    public InverseObjectProperties (hydra.ext.owl.syntax.InverseObjectProperties value) {
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
  
  public static final class IrreflexiveObjectProperty extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.IrreflexiveObjectProperty value;
    
    public IrreflexiveObjectProperty (hydra.ext.owl.syntax.IrreflexiveObjectProperty value) {
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
  
  public static final class ObjectPropertyDomain extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.ObjectPropertyDomain value;
    
    public ObjectPropertyDomain (hydra.ext.owl.syntax.ObjectPropertyDomain value) {
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
  
  public static final class ObjectPropertyRange extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.ObjectPropertyRange value;
    
    public ObjectPropertyRange (hydra.ext.owl.syntax.ObjectPropertyRange value) {
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
  
  public static final class ReflexiveObjectProperty extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.ReflexiveObjectProperty value;
    
    public ReflexiveObjectProperty (hydra.ext.owl.syntax.ReflexiveObjectProperty value) {
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
  
  public static final class SubObjectPropertyOf extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.SubObjectPropertyOf value;
    
    public SubObjectPropertyOf (hydra.ext.owl.syntax.SubObjectPropertyOf value) {
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
  
  public static final class SymmetricObjectProperty extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.SymmetricObjectProperty value;
    
    public SymmetricObjectProperty (hydra.ext.owl.syntax.SymmetricObjectProperty value) {
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
  
  public static final class TransitiveObjectProperty extends hydra.ext.owl.syntax.ObjectPropertyAxiom {
    public final hydra.ext.owl.syntax.TransitiveObjectProperty value;
    
    public TransitiveObjectProperty (hydra.ext.owl.syntax.TransitiveObjectProperty value) {
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