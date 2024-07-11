// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * An import statement
 */
public class Import implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.Import");
  
  public final Boolean qualified;
  
  public final hydra.langs.haskell.ast.ModuleName module;
  
  public final hydra.util.Opt<hydra.langs.haskell.ast.ModuleName> as;
  
  public final hydra.util.Opt<hydra.langs.haskell.ast.Import_Spec> spec;
  
  public Import (Boolean qualified, hydra.langs.haskell.ast.ModuleName module, hydra.util.Opt<hydra.langs.haskell.ast.ModuleName> as, hydra.util.Opt<hydra.langs.haskell.ast.Import_Spec> spec) {
    java.util.Objects.requireNonNull((qualified));
    java.util.Objects.requireNonNull((module));
    java.util.Objects.requireNonNull((as));
    java.util.Objects.requireNonNull((spec));
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
    java.util.Objects.requireNonNull((qualified));
    return new Import(qualified, module, as, spec);
  }
  
  public Import withModule(hydra.langs.haskell.ast.ModuleName module) {
    java.util.Objects.requireNonNull((module));
    return new Import(qualified, module, as, spec);
  }
  
  public Import withAs(hydra.util.Opt<hydra.langs.haskell.ast.ModuleName> as) {
    java.util.Objects.requireNonNull((as));
    return new Import(qualified, module, as, spec);
  }
  
  public Import withSpec(hydra.util.Opt<hydra.langs.haskell.ast.Import_Spec> spec) {
    java.util.Objects.requireNonNull((spec));
    return new Import(qualified, module, as, spec);
  }
}