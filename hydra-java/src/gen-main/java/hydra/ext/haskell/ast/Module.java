package hydra.ext.haskell.ast;

public class Module {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Module");
  
  public final java.util.Optional<hydra.ext.haskell.ast.ModuleHead> head;
  
  public final java.util.List<hydra.ext.haskell.ast.Import> imports;
  
  public final java.util.List<hydra.ext.haskell.ast.DeclarationWithComments> declarations;
  
  public Module (java.util.Optional<hydra.ext.haskell.ast.ModuleHead> head, java.util.List<hydra.ext.haskell.ast.Import> imports, java.util.List<hydra.ext.haskell.ast.DeclarationWithComments> declarations) {
    this.head = head;
    this.imports = imports;
    this.declarations = declarations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Module)) {
      return false;
    }
    Module o = (Module) (other);
    return head.equals(o.head) && imports.equals(o.imports) && declarations.equals(o.declarations);
  }
  
  @Override
  public int hashCode() {
    return 2 * head.hashCode() + 3 * imports.hashCode() + 5 * declarations.hashCode();
  }
  
  public Module withHead(java.util.Optional<hydra.ext.haskell.ast.ModuleHead> head) {
    return new Module(head, imports, declarations);
  }
  
  public Module withImports(java.util.List<hydra.ext.haskell.ast.Import> imports) {
    return new Module(head, imports, declarations);
  }
  
  public Module withDeclarations(java.util.List<hydra.ext.haskell.ast.DeclarationWithComments> declarations) {
    return new Module(head, imports, declarations);
  }
}