// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class TargetWithStarAtom implements Serializable, Comparable<TargetWithStarAtom> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TargetWithStarAtom");
  
  public static final hydra.core.Name FIELD_NAME_PROJECT = new hydra.core.Name("project");
  
  public static final hydra.core.Name FIELD_NAME_SLICES = new hydra.core.Name("slices");
  
  public static final hydra.core.Name FIELD_NAME_ATOM = new hydra.core.Name("atom");
  
  private TargetWithStarAtom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Project instance) ;
    
    R visit(Slices instance) ;
    
    R visit(Atom instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TargetWithStarAtom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Project instance) {
      return otherwise(instance);
    }
    
    default R visit(Slices instance) {
      return otherwise(instance);
    }
    
    default R visit(Atom instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Project extends hydra.ext.python.syntax.TargetWithStarAtom implements Serializable {
    public final hydra.ext.python.syntax.TPrimaryAndName value;
    
    public Project (hydra.ext.python.syntax.TPrimaryAndName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Project)) {
        return false;
      }
      Project o = (Project) other;
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
    public int compareTo(TargetWithStarAtom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Project o = (Project) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Slices extends hydra.ext.python.syntax.TargetWithStarAtom implements Serializable {
    public final hydra.ext.python.syntax.TPrimaryAndSlices value;
    
    public Slices (hydra.ext.python.syntax.TPrimaryAndSlices value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Slices)) {
        return false;
      }
      Slices o = (Slices) other;
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
    public int compareTo(TargetWithStarAtom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Slices o = (Slices) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Atom extends hydra.ext.python.syntax.TargetWithStarAtom implements Serializable {
    public final hydra.ext.python.syntax.StarAtom value;
    
    public Atom (hydra.ext.python.syntax.StarAtom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Atom)) {
        return false;
      }
      Atom o = (Atom) other;
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
    public int compareTo(TargetWithStarAtom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Atom o = (Atom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
