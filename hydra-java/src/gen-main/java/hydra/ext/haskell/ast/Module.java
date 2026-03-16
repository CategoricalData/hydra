// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A Haskell module
 */
public class Module implements Serializable, Comparable<Module> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.ast.Module");
  
  public static final hydra.core.Name HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name IMPORTS = new hydra.core.Name("imports");
  
  public static final hydra.core.Name DECLARATIONS = new hydra.core.Name("declarations");
  
  /**
   * Optional module head
   */
  public final hydra.util.Maybe<hydra.ext.haskell.ast.ModuleHead> head;
  
  /**
   * Import statements
   */
  public final hydra.util.ConsList<hydra.ext.haskell.ast.Import> imports;
  
  /**
   * Module declarations
   */
  public final hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments> declarations;
  
  public Module (hydra.util.Maybe<hydra.ext.haskell.ast.ModuleHead> head, hydra.util.ConsList<hydra.ext.haskell.ast.Import> imports, hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments> declarations) {
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
    cmp = ((Comparable) head).compareTo(other.head);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) imports).compareTo(other.imports);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) declarations).compareTo(other.declarations);
  }
  
  public Module withHead(hydra.util.Maybe<hydra.ext.haskell.ast.ModuleHead> head) {
    return new Module(head, imports, declarations);
  }
  
  public Module withImports(hydra.util.ConsList<hydra.ext.haskell.ast.Import> imports) {
    return new Module(head, imports, declarations);
  }
  
  public Module withDeclarations(hydra.util.ConsList<hydra.ext.haskell.ast.DeclarationWithComments> declarations) {
    return new Module(head, imports, declarations);
  }
}
