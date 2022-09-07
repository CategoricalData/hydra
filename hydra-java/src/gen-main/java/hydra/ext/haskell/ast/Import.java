package hydra.ext.haskell.ast;

/**
 * An import statement
 */
public class Import {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Import");
  
  public final Boolean qualified;
  
  public final hydra.ext.haskell.ast.ModuleName module;
  
  public final java.util.Optional<hydra.ext.haskell.ast.ModuleName> as;
  
  public final java.util.Optional<hydra.ext.haskell.ast.Import_Spec> spec;
  
  public Import (Boolean qualified, hydra.ext.haskell.ast.ModuleName module, java.util.Optional<hydra.ext.haskell.ast.ModuleName> as, java.util.Optional<hydra.ext.haskell.ast.Import_Spec> spec) {
    this.qualified = qualified;
    this.module = module;
    this.as = as;
    this.spec = spec;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Import)) {
      return false;
    }
    Import o = (Import) (other);
    return qualified.equals(o.qualified) && module.equals(o.module) && as.equals(o.as) && spec.equals(o.spec);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualified.hashCode() + 3 * module.hashCode() + 5 * as.hashCode() + 7 * spec.hashCode();
  }
  
  public Import withQualified(Boolean qualified) {
    return new Import(qualified, module, as, spec);
  }
  
  public Import withModule(hydra.ext.haskell.ast.ModuleName module) {
    return new Import(qualified, module, as, spec);
  }
  
  public Import withAs(java.util.Optional<hydra.ext.haskell.ast.ModuleName> as) {
    return new Import(qualified, module, as, spec);
  }
  
  public Import withSpec(java.util.Optional<hydra.ext.haskell.ast.Import_Spec> spec) {
    return new Import(qualified, module, as, spec);
  }
}