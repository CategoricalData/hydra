// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class CompilationUnit implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CompilationUnit");
  
  public static final hydra.core.Name FIELD_NAME_ORDINARY = new hydra.core.Name("ordinary");
  
  public static final hydra.core.Name FIELD_NAME_MODULAR = new hydra.core.Name("modular");
  
  private CompilationUnit () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ordinary instance) ;
    
    R visit(Modular instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CompilationUnit instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ordinary instance) {
      return otherwise((instance));
    }
    
    default R visit(Modular instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ordinary extends hydra.ext.java.syntax.CompilationUnit implements Serializable {
    public final hydra.ext.java.syntax.OrdinaryCompilationUnit value;
    
    public Ordinary (hydra.ext.java.syntax.OrdinaryCompilationUnit value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ordinary)) {
        return false;
      }
      Ordinary o = (Ordinary) (other);
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
  
  public static final class Modular extends hydra.ext.java.syntax.CompilationUnit implements Serializable {
    public final hydra.ext.java.syntax.ModularCompilationUnit value;
    
    public Modular (hydra.ext.java.syntax.ModularCompilationUnit value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Modular)) {
        return false;
      }
      Modular o = (Modular) (other);
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