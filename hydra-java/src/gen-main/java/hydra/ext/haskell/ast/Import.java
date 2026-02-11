// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An import statement
 */
public class Import implements Serializable, Comparable<Import> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.Import");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED = new hydra.core.Name("qualified");
  
  public static final hydra.core.Name FIELD_NAME_MODULE = new hydra.core.Name("module");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public static final hydra.core.Name FIELD_NAME_SPEC = new hydra.core.Name("spec");
  
  /**
   * Whether the import is qualified
   */
  public final Boolean qualified;
  
  /**
   * The module being imported
   */
  public final hydra.ext.haskell.ast.ModuleName module;
  
  /**
   * Optional alias for the module
   */
  public final hydra.util.Maybe<hydra.ext.haskell.ast.ModuleName> as;
  
  /**
   * Optional import specification
   */
  public final hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport> spec;
  
  public Import (Boolean qualified, hydra.ext.haskell.ast.ModuleName module, hydra.util.Maybe<hydra.ext.haskell.ast.ModuleName> as, hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport> spec) {
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
    Import o = (Import) other;
    return java.util.Objects.equals(
      this.qualified,
      o.qualified) && java.util.Objects.equals(
      this.module,
      o.module) && java.util.Objects.equals(
      this.as,
      o.as) && java.util.Objects.equals(
      this.spec,
      o.spec);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualified) + 3 * java.util.Objects.hashCode(module) + 5 * java.util.Objects.hashCode(as) + 7 * java.util.Objects.hashCode(spec);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Import other) {
    int cmp = 0;
    cmp = ((Comparable) qualified).compareTo(other.qualified);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) module).compareTo(other.module);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      as.hashCode(),
      other.as.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      spec.hashCode(),
      other.spec.hashCode());
  }
  
  public Import withQualified(Boolean qualified) {
    return new Import(qualified, module, as, spec);
  }
  
  public Import withModule(hydra.ext.haskell.ast.ModuleName module) {
    return new Import(qualified, module, as, spec);
  }
  
  public Import withAs(hydra.util.Maybe<hydra.ext.haskell.ast.ModuleName> as) {
    return new Import(qualified, module, as, spec);
  }
  
  public Import withSpec(hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport> spec) {
    return new Import(qualified, module, as, spec);
  }
}
