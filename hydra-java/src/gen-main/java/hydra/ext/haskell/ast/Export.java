package hydra.ext.haskell.ast;

/**
 * An export statement
 */
public abstract class Export {
  private Export () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Declaration instance) ;
    
    R visit(Module instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Export instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Declaration instance) {
      return otherwise((instance));
    }
    
    default R visit(Module instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Declaration extends hydra.ext.haskell.ast.Export {
    public final hydra.ext.haskell.ast.ImportExportSpec value;
    
    public Declaration (hydra.ext.haskell.ast.ImportExportSpec value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Declaration)) {
        return false;
      }
      Declaration o = (Declaration) (other);
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
  
  public static final class Module extends hydra.ext.haskell.ast.Export {
    public final hydra.ext.haskell.ast.ModuleName value;
    
    public Module (hydra.ext.haskell.ast.ModuleName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Module)) {
        return false;
      }
      Module o = (Module) (other);
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