package hydra.ext.java.syntax;

public abstract class CompilationUnit {
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
  
  public static final class Ordinary extends hydra.ext.java.syntax.CompilationUnit {
    public final hydra.ext.java.syntax.OrdinaryCompilationUnit value;
    
    public Ordinary (hydra.ext.java.syntax.OrdinaryCompilationUnit value) {
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
  
  public static final class Modular extends hydra.ext.java.syntax.CompilationUnit {
    public final hydra.ext.java.syntax.ModularCompilationUnit value;
    
    public Modular (hydra.ext.java.syntax.ModularCompilationUnit value) {
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