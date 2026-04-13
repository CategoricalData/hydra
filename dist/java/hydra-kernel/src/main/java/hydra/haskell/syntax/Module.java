// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A Haskell module
 */
public class Module implements Serializable, Comparable<Module> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.Module");

  public static final hydra.core.Name HEAD = new hydra.core.Name("head");

  public static final hydra.core.Name IMPORTS = new hydra.core.Name("imports");

  public static final hydra.core.Name DECLARATIONS = new hydra.core.Name("declarations");

  /**
   * Optional module head
   */
  public final hydra.util.Maybe<hydra.haskell.syntax.ModuleHead> head;

  /**
   * Import statements
   */
  public final java.util.List<hydra.haskell.syntax.Import> imports;

  /**
   * Module declarations
   */
  public final java.util.List<hydra.haskell.syntax.DeclarationWithComments> declarations;

  public Module (hydra.util.Maybe<hydra.haskell.syntax.ModuleHead> head, java.util.List<hydra.haskell.syntax.Import> imports, java.util.List<hydra.haskell.syntax.DeclarationWithComments> declarations) {
    this.head = head;
    this.imports = imports;
    this.declarations = declarations;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Module)) {
      return false;
    }
    Module o = (Module) other;
    return java.util.Objects.equals(
      this.head,
      o.head) && java.util.Objects.equals(
      this.imports,
      o.imports) && java.util.Objects.equals(
      this.declarations,
      o.declarations);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(head) + 3 * java.util.Objects.hashCode(imports) + 5 * java.util.Objects.hashCode(declarations);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Module other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      head,
      other.head);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      imports,
      other.imports);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      declarations,
      other.declarations);
  }

  public Module withHead(hydra.util.Maybe<hydra.haskell.syntax.ModuleHead> head) {
    return new Module(head, imports, declarations);
  }

  public Module withImports(java.util.List<hydra.haskell.syntax.Import> imports) {
    return new Module(head, imports, declarations);
  }

  public Module withDeclarations(java.util.List<hydra.haskell.syntax.DeclarationWithComments> declarations) {
    return new Module(head, imports, declarations);
  }
}
