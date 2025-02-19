// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class ModuleHead implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ModuleHead");
  
  public static final hydra.core.Name FIELD_NAME_COMMENTS = new hydra.core.Name("comments");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_EXPORTS = new hydra.core.Name("exports");
  
  public final hydra.util.Opt<String> comments;
  
  public final hydra.ext.haskell.ast.ModuleName name;
  
  public final java.util.List<hydra.ext.haskell.ast.Export> exports;
  
  public ModuleHead (hydra.util.Opt<String> comments, hydra.ext.haskell.ast.ModuleName name, java.util.List<hydra.ext.haskell.ast.Export> exports) {
    java.util.Objects.requireNonNull((comments));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((exports));
    this.comments = comments;
    this.name = name;
    this.exports = exports;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleHead)) {
      return false;
    }
    ModuleHead o = (ModuleHead) (other);
    return comments.equals(o.comments) && name.equals(o.name) && exports.equals(o.exports);
  }
  
  @Override
  public int hashCode() {
    return 2 * comments.hashCode() + 3 * name.hashCode() + 5 * exports.hashCode();
  }
  
  public ModuleHead withComments(hydra.util.Opt<String> comments) {
    java.util.Objects.requireNonNull((comments));
    return new ModuleHead(comments, name, exports);
  }
  
  public ModuleHead withName(hydra.ext.haskell.ast.ModuleName name) {
    java.util.Objects.requireNonNull((name));
    return new ModuleHead(comments, name, exports);
  }
  
  public ModuleHead withExports(java.util.List<hydra.ext.haskell.ast.Export> exports) {
    java.util.Objects.requireNonNull((exports));
    return new ModuleHead(comments, name, exports);
  }
}