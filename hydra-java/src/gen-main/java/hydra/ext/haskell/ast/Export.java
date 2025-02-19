// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An export statement
 */
public abstract class Export implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.Export");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATION = new hydra.core.Name("declaration");
  
  public static final hydra.core.Name FIELD_NAME_MODULE = new hydra.core.Name("module");
  
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
  
  public static final class Declaration extends hydra.ext.haskell.ast.Export implements Serializable {
    public final hydra.ext.haskell.ast.ImportExportSpec value;
    
    public Declaration (hydra.ext.haskell.ast.ImportExportSpec value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Module extends hydra.ext.haskell.ast.Export implements Serializable {
    public final hydra.ext.haskell.ast.ModuleName value;
    
    public Module (hydra.ext.haskell.ast.ModuleName value) {
      java.util.Objects.requireNonNull((value));
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