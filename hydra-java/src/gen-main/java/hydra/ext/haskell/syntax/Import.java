// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * An import statement
 */
public class Import implements Serializable, Comparable<Import> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.Import");

  public static final hydra.core.Name QUALIFIED = new hydra.core.Name("qualified");

  public static final hydra.core.Name MODULE = new hydra.core.Name("module");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public static final hydra.core.Name SPEC = new hydra.core.Name("spec");

  /**
   * Whether the import is qualified
   */
  public final Boolean qualified;

  /**
   * The module being imported
   */
  public final hydra.ext.haskell.syntax.ModuleName module;

  /**
   * Optional alias for the module
   */
  public final hydra.util.Maybe<hydra.ext.haskell.syntax.ModuleName> as;

  /**
   * Optional import specification
   */
  public final hydra.util.Maybe<hydra.ext.haskell.syntax.SpecImport> spec;

  public Import (Boolean qualified, hydra.ext.haskell.syntax.ModuleName module, hydra.util.Maybe<hydra.ext.haskell.syntax.ModuleName> as, hydra.util.Maybe<hydra.ext.haskell.syntax.SpecImport> spec) {
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
    cmp = ((Comparable) as).compareTo(other.as);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) spec).compareTo(other.spec);
  }

  public Import withQualified(Boolean qualified) {
    return new Import(qualified, module, as, spec);
  }

  public Import withModule(hydra.ext.haskell.syntax.ModuleName module) {
    return new Import(qualified, module, as, spec);
  }

  public Import withAs(hydra.util.Maybe<hydra.ext.haskell.syntax.ModuleName> as) {
    return new Import(qualified, module, as, spec);
  }

  public Import withSpec(hydra.util.Maybe<hydra.ext.haskell.syntax.SpecImport> spec) {
    return new Import(qualified, module, as, spec);
  }
}
