// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class PrimaryRhs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.PrimaryRhs");
  
  public static final hydra.core.Name FIELD_NAME_PROJECT = new hydra.core.Name("project");
  
  public static final hydra.core.Name FIELD_NAME_GENEXP = new hydra.core.Name("genexp");
  
  public static final hydra.core.Name FIELD_NAME_CALL = new hydra.core.Name("call");
  
  public static final hydra.core.Name FIELD_NAME_SLICES = new hydra.core.Name("slices");
  
  private PrimaryRhs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Project instance) ;
    
    R visit(Genexp instance) ;
    
    R visit(Call instance) ;
    
    R visit(Slices instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimaryRhs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Project instance) {
      return otherwise((instance));
    }
    
    default R visit(Genexp instance) {
      return otherwise((instance));
    }
    
    default R visit(Call instance) {
      return otherwise((instance));
    }
    
    default R visit(Slices instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Project extends hydra.ext.python.syntax.PrimaryRhs implements Serializable {
    public final hydra.ext.python.syntax.Name value;
    
    public Project (hydra.ext.python.syntax.Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Project)) {
        return false;
      }
      Project o = (Project) (other);
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
  
  public static final class Genexp extends hydra.ext.python.syntax.PrimaryRhs implements Serializable {
    public final hydra.ext.python.syntax.Genexp value;
    
    public Genexp (hydra.ext.python.syntax.Genexp value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Genexp)) {
        return false;
      }
      Genexp o = (Genexp) (other);
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
  
  public static final class Call extends hydra.ext.python.syntax.PrimaryRhs implements Serializable {
    public final hydra.ext.python.syntax.Args value;
    
    public Call (hydra.ext.python.syntax.Args value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Call)) {
        return false;
      }
      Call o = (Call) (other);
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
  
  public static final class Slices extends hydra.ext.python.syntax.PrimaryRhs implements Serializable {
    public final hydra.ext.python.syntax.Slices value;
    
    public Slices (hydra.ext.python.syntax.Slices value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Slices)) {
        return false;
      }
      Slices o = (Slices) (other);
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