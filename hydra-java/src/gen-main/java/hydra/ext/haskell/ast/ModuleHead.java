// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A module head
 */
public class ModuleHead implements Serializable, Comparable<ModuleHead> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ModuleHead");
  
  public static final hydra.core.Name FIELD_NAME_COMMENTS = new hydra.core.Name("comments");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_EXPORTS = new hydra.core.Name("exports");
  
  /**
   * Optional module-level comments
   */
  public final hydra.util.Maybe<String> comments;
  
  /**
   * The module name
   */
  public final hydra.ext.haskell.ast.ModuleName name;
  
  /**
   * Export list
   */
  public final java.util.List<hydra.ext.haskell.ast.Export> exports;
  
  public ModuleHead (hydra.util.Maybe<String> comments, hydra.ext.haskell.ast.ModuleName name, java.util.List<hydra.ext.haskell.ast.Export> exports) {
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
    return java.util.Objects.equals(
      this.comments,
      o.comments) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.exports,
      o.exports);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(comments) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(exports);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleHead other) {
    int cmp = 0;
    cmp = Integer.compare(
      comments.hashCode(),
      other.comments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      exports.hashCode(),
      other.exports.hashCode());
  }
  
  public ModuleHead withComments(hydra.util.Maybe<String> comments) {
    return new ModuleHead(comments, name, exports);
  }
  
  public ModuleHead withName(hydra.ext.haskell.ast.ModuleName name) {
    return new ModuleHead(comments, name, exports);
  }
  
  public ModuleHead withExports(java.util.List<hydra.ext.haskell.ast.Export> exports) {
    return new ModuleHead(comments, name, exports);
  }
}
