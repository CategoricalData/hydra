package hydra.ext.owl.syntax;

public abstract class ClassAxiom {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.ClassAxiom");
  
  private ClassAxiom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(DisjointClasses instance) ;
    
    R visit(DisjointUnion instance) ;
    
    R visit(EquivalentClasses instance) ;
    
    R visit(SubClassOf instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassAxiom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(DisjointClasses instance) {
      return otherwise((instance));
    }
    
    default R visit(DisjointUnion instance) {
      return otherwise((instance));
    }
    
    default R visit(EquivalentClasses instance) {
      return otherwise((instance));
    }
    
    default R visit(SubClassOf instance) {
      return otherwise((instance));
    }
  }
  
  public static final class DisjointClasses extends hydra.ext.owl.syntax.ClassAxiom {
    public final hydra.ext.owl.syntax.DisjointClasses value;
    
    public DisjointClasses (hydra.ext.owl.syntax.DisjointClasses value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DisjointClasses)) {
        return false;
      }
      DisjointClasses o = (DisjointClasses) (other);
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
  
  public static final class DisjointUnion extends hydra.ext.owl.syntax.ClassAxiom {
    public final hydra.ext.owl.syntax.DisjointUnion value;
    
    public DisjointUnion (hydra.ext.owl.syntax.DisjointUnion value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DisjointUnion)) {
        return false;
      }
      DisjointUnion o = (DisjointUnion) (other);
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
  
  public static final class EquivalentClasses extends hydra.ext.owl.syntax.ClassAxiom {
    public final hydra.ext.owl.syntax.EquivalentClasses value;
    
    public EquivalentClasses (hydra.ext.owl.syntax.EquivalentClasses value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EquivalentClasses)) {
        return false;
      }
      EquivalentClasses o = (EquivalentClasses) (other);
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
  
  public static final class SubClassOf extends hydra.ext.owl.syntax.ClassAxiom {
    public final hydra.ext.owl.syntax.SubClassOf value;
    
    public SubClassOf (hydra.ext.owl.syntax.SubClassOf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubClassOf)) {
        return false;
      }
      SubClassOf o = (SubClassOf) (other);
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